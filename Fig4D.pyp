import c4d #pylint: disable=import-error
import json
import os
import re
import requests #pylint: disable=import-error
import urllib
from xml.dom import minidom

PLUGIN_ID = 1054883

class FigmaHelper():
    _C4DDOC = None
    _FILE = None
    _TOKEN = None
    _DEPTH = 10.0

    def SyncDoc(self):
        self._C4DDOC.StartUndo()

        uxFile = self.FetchFigmaDoc()
        frames = self.RecurseFigmaDoc(uxFile['document']['children'][0]['children'])
        for frame in reversed(frames):
            self._C4DDOC.InsertObject(frame)
            self._C4DDOC.AddUndo(c4d.UNDOTYPE_NEW, frame)

        self._C4DDOC.EndUndo()
        c4d.EventAdd()

        return True

    def FetchFigmaDoc(self):
        req = requests.get(
            'https://api.figma.com/v1/files/' + self._FILE, 
            headers={'X-Figma-Token': self._TOKEN},
            params={'geometry': 'paths' }
        )
        return req.json()

    def InsertLayerUnder(self, layer, parent):
        pos = layer.GetAbsPos()
        pos -= parent.GetAbsPos()

        layer.InsertUnder(parent)
        layer.SetAbsPos(pos)

    def SplineFromPath(self, path):
        closed = False
        segs = []
        pntCount = 0

        cursor = c4d.Vector(0.0)
        commands = re.findall(r'[A-Za-z][-0-9\.\s]*', str(path))
        for command in commands:
            cType = command[0]
            coords = re.split(r"\s*", command[1:])
            if cType == "M" or cType == "m":
                segs.append({
                    'points': [],
                    'vl': [],
                    'vr': [],
                })

                for x in range(0, len(coords), 2):
                    y = x + 1
                    if cType == "m":
                        cursor += c4d.Vector(float(coords[x]), -float(coords[y]), 0.0)
                    else:
                        cursor = c4d.Vector(float(coords[x]), -float(coords[y]), 0.0)
                    segs[-1]['points'].append(cursor)
                    segs[-1]['vl'].append(c4d.Vector(0))
                    segs[-1]['vr'].append(c4d.Vector(0))
                    pntCount += 1
            if cType == "L" or cType == "l":
                for x in range(0, len(coords), 2):
                    y = x + 1
                    if cType == "l":
                        cursor += c4d.Vector(float(coords[x]), -float(coords[y]), 0.0)
                    else:
                        cursor = c4d.Vector(float(coords[x]), -float(coords[y]), 0.0)
                    segs[-1]['points'].append(cursor)
                    segs[-1]['vl'].append(c4d.Vector(0))
                    segs[-1]['vr'].append(c4d.Vector(0))
                    pntCount += 1
            if cType == "H" or cType == "h":
                for x in range(len(coords)):
                    if cType == "h":
                        cursor += c4d.Vector(float(coords[x]), 0.0, 0.0)
                    else:
                        cursor = c4d.Vector(float(coords[x]), cursor.y, 0.0)
                    segs[-1]['points'].append(cursor)
                    segs[-1]['vl'].append(c4d.Vector(0))
                    segs[-1]['vr'].append(c4d.Vector(0))
                    pntCount += 1
            if cType == "V" or cType == "v":
                for y in range(len(coords)):
                    if cType == "v":
                        cursor += c4d.Vector(0.0, -float(coords[y]), 0.0)
                    else:
                        cursor = c4d.Vector(cursor.x, -float(coords[y]), 0.0)
                    segs[-1]['points'].append(cursor)
                    segs[-1]['vl'].append(c4d.Vector(0))
                    segs[-1]['vr'].append(c4d.Vector(0))
                    pntCount += 1
            if cType == "C" or cType == "c":
                for x in range(0, len(coords), 6):
                    c4dBezierComp = 0.75
                    vr = c4d.Vector(float(coords[x]), -float(coords[x + 1]), 0.0) - cursor
                    vr = vr.GetNormalized() * (vr.GetLength() * c4dBezierComp)
                    segs[-1]['vr'][-1] = vr
                    if cType == "c":
                        cursor += c4d.Vector(float(coords[x + 4]), -float(coords[x + 5]), 0.0)
                    else:
                        cursor = c4d.Vector(float(coords[x + 4]), -float(coords[x + 5]), 0.0)
                    segs[-1]['points'].append(cursor)
                    vl = c4d.Vector(float(coords[x + 2]), -float(coords[x + 3]), 0.0) - cursor
                    vl = vl.GetNormalized() * (vl.GetLength() * c4dBezierComp)
                    segs[-1]['vl'].append(vl)
                    segs[-1]['vr'].append(c4d.Vector(0))
                    pntCount += 1
            if cType == "Z":
                closed = True

        spline = c4d.SplineObject(0, c4d.SPLINETYPE_BEZIER)
        spline.ResizeObject(pntCount, len(segs))
        segId = 0
        pntId = 0
        for seg in segs:
            segCount = len(seg['points'])
            spline.SetSegment(segId, segCount, closed)
            segId += 1
            for x in range(segCount):
                p = seg['points'][x]
                vl = seg['vl'][x]
                vr = seg['vr'][x]
                spline.SetPoint(pntId, p)
                spline.SetTangent(pntId, vl, vr)
                pntId += 1

        spline[c4d.SPLINEOBJECT_CLOSED] = closed
        spline.Message(c4d.MSG_UPDATE)
            
        return spline

    def ConnectSplines(self, splines):
        pntCnt = 0
        segCnt = 0
        closed = False

        for spline in splines:
            thisSegCnt = spline.GetSegmentCount()
            if thisSegCnt == 0: thisSegCnt = 1
            segCnt += thisSegCnt
            pntCnt += spline.GetPointCount()

        newSpline = c4d.SplineObject(0, c4d.SPLINETYPE_BEZIER)
        newSpline.ResizeObject(pntCnt, segCnt)

        pntOff = 0
        segOff = 0
        tanOff = 0
        for x in range(len(splines)):
            spline = splines[x]
            if spline.IsClosed(): closed = True
            pnts = spline.GetPointCount()
            segs = spline.GetSegmentCount()
            tans = spline.GetTangentCount()

            for y in range(pnts):
                pnt = spline.GetPoint(y)
                newSpline.SetPoint(pntOff + y, pnt)

            if segs > 0:
                for y in range(segs):
                    seg = spline.GetSegment(y)
                    newSpline.SetSegment(segOff + y, seg['cnt'], seg['closed'])
            else:
                newSpline.SetSegment(segOff, pnts, spline.IsClosed())

            for y in range(tans):
                tan = spline.GetTangent(y)
                newSpline.SetTangent(tanOff + y, tan['vl'], tan['vr'])

            pntOff += pnts
            if segs == 0: segs = 1
            segOff += segs
            tanOff += tans

        newSpline[c4d.SPLINEOBJECT_CLOSED] = closed
        newSpline.Message(c4d.MSG_UPDATE)

        return newSpline

    def CreateNodeGroup(self, node):
        null = c4d.BaseObject(c4d.Onull)
        absBBox = node['absoluteBoundingBox']
        nodeTrans = node['relativeTransform']

        null.SetName(node['name'])

        nodeMar = c4d.Matrix(
            off=c4d.Vector(absBBox['x'], -absBBox['y'], 0.0),
            v1=c4d.Vector(nodeTrans[0][0], nodeTrans[0][1], 0),
            v2=c4d.Vector(nodeTrans[1][0], nodeTrans[1][1], 0),
            v3=c4d.Vector(0, 0, 1),
        )
        null.SetMg(nodeMar)

        nodeKeys = node.keys()

        if 'visible' in nodeKeys:
            if not node['visible']:
                null.SetEditorMode(c4d.MODE_OFF)
                null.SetRenderMode(c4d.MODE_OFF)

        if 'opacity' in nodeKeys:
            disTag = c4d.BaseTag(c4d.Tdisplay)
            disTag[c4d.DISPLAYTAG_AFFECT_VISIBILITY] = True
            disTag[c4d.DISPLAYTAG_VISIBILITY] = float(node['opacity'])
            null.InsertTag(disTag)

        if 'locked' in nodeKeys:
            if bool(node['locked']):
                proTag = c4d.BaseTag(c4d.Tprotection)
                null.InsertTag(proTag)
        
        return null

    def CreateFill(self, geo, fills, name):
        extrude = c4d.BaseObject(c4d.Oextrude)
        extrude[c4d.EXTRUDEOBJECT_MOVE] = c4d.Vector(0, 0, self._DEPTH)

        for x in range(len(fills) - 1, -1, -1):
            fill = fills[x]

            mat = c4d.Material()
            mat.SetName(str(name) + " Fill " + str(x + 1))

            fillKeys = fill.keys()
            
            if fill['type'] == 'SOLID':
                color = fill['color']
                mat[c4d.MATERIAL_COLOR_COLOR] = c4d.Vector(color['r'], color['g'], color['b'])

                if 'opacity' in fillKeys:
                    mat.SetChannelState(c4d.CHANNEL_ALPHA, True)
                    aShader = c4d.BaseShader(c4d.Xcolor)
                    aShader[c4d.COLORSHADER_COLOR] = c4d.Vector(fill['opacity'])
                    mat[c4d.MATERIAL_ALPHA_SHADER] = aShader
                    mat.InsertShader(aShader)
            
            self._C4DDOC.InsertMaterial(mat)
            self._C4DDOC.AddUndo(c4d.UNDOTYPE_NEW, mat)

            if not 'visible' in fillKeys: fill['visible'] = True
            if bool(fill['visible']):
                tex = extrude.MakeTag(c4d.Ttexture)
                tex[c4d.TEXTURETAG_PROJECTION] = 6 # UVW Mapping
                tex.SetMaterial(mat)

        geoClone = geo.GetClone()
        geoClone.InsertUnder(extrude)

        return extrude
    
    def CreateStrokeProfile(self, weight, alignment, name):
        profile = c4d.SplineObject(4, c4d.SPLINETYPE_LINEAR)
        profile.SetName(name + " Profile")
        profile[c4d.SPLINEOBJECT_CLOSED] = True

        p = []
        if alignment == 'OUTSIDE':
            p.append(c4d.Vector(0))
            p.append(c4d.Vector(float(weight), 0, 0))
            p.append(c4d.Vector(float(weight), -self._DEPTH, 0))
            p.append(c4d.Vector(0, -self._DEPTH, 0))
        elif alignment == 'INSIDE':
            p.append(c4d.Vector(0))
            p.append(c4d.Vector(-float(weight), 0, 0))
            p.append(c4d.Vector(-float(weight), -self._DEPTH, 0))
            p.append(c4d.Vector(0, -self._DEPTH, 0))
        else:
            halfWidth = float(weight) / 2
            p.append(c4d.Vector(-halfWidth, 0, 0))
            p.append(c4d.Vector(halfWidth, 0, 0))
            p.append(c4d.Vector(halfWidth, -self._DEPTH, 0))
            p.append(c4d.Vector(-halfWidth, -self._DEPTH, 0))
        profile.SetAllPoints(p)
        profile.Message(c4d.MSG_UPDATE)

        return profile

    def CreateStroke(self, geo, strokes, weight, alignment, name):
        sweep = c4d.BaseObject(c4d.Osweep)
        profile = self.CreateStrokeProfile(weight, alignment, name)

        for x in range(len(strokes) - 1, -1, -1):
            stroke = strokes[x]

            mat = c4d.Material()
            mat.SetName(str(name) + " Stroke " + str(x + 1))

            strokeKeys = stroke.keys()
            
            if stroke['type'] == 'SOLID':
                color = stroke['color']
                mat[c4d.MATERIAL_COLOR_COLOR] = c4d.Vector(color['r'], color['g'], color['b'])

                if 'opacity' in strokeKeys:
                    mat.SetChannelState(c4d.CHANNEL_ALPHA, True)
                    aShader = c4d.BaseShader(c4d.Xcolor)
                    aShader[c4d.COLORSHADER_COLOR] = c4d.Vector(stroke['opacity'])
                    mat[c4d.MATERIAL_ALPHA_SHADER] = aShader
                    mat.InsertShader(aShader)
            
            self._C4DDOC.InsertMaterial(mat)
            self._C4DDOC.AddUndo(c4d.UNDOTYPE_NEW, mat)

            if not 'visible' in strokeKeys: stroke['visible'] = True
            if bool(stroke['visible']):
                tex = sweep.MakeTag(c4d.Ttexture)
                tex[c4d.TEXTURETAG_PROJECTION] = 6 # UVW Mapping
                tex.SetMaterial(mat)

        geoClone = geo.GetClone()
        geoClone.InsertUnder(sweep)
        profile.InsertUnder(sweep)

        return sweep

    def CreateVector(self, node):
        group = self.CreateNodeGroup(node)

        smcSettings = c4d.BaseContainer()
        smcSettings[c4d.MDATA_OPTIMIZE_TOLERANCE] = 0.001
        smcSettings[c4d.MDATA_OPTIMIZE_POINTS] = True
        smcSettings[c4d.MDATA_OPTIMIZE_POLYGONS] = False
        smcSettings[c4d.MDATA_OPTIMIZE_UNUSEDPOINTS] = True

        fillGeo = []
        strokeGeo = []

        # Get geo from fill
        if len(node['fillGeometry']) > 0:
            for x in node['fillGeometry']:
                spline = self.SplineFromPath(x['path'])
                c4d.utils.SendModelingCommand(
                    command=c4d.MCOMMAND_OPTIMIZE,
                    list=[spline],
                    mode=c4d.MODELINGCOMMANDMODE_ALL,
                    bc=smcSettings,
                )
                spline.SetName(node['name'] + " Fill Geometry")
                fillGeo.append(spline)
        fillGeo = self.ConnectSplines(fillGeo)
        
        # Get geo from stroke
        if len(node['strokeGeometry']) > 0:
            for x in node['strokeGeometry']:
                spline = self.SplineFromPath(x['path'])
                c4d.utils.SendModelingCommand(
                    command=c4d.MCOMMAND_OPTIMIZE,
                    list=[spline],
                    mode=c4d.MODELINGCOMMANDMODE_ALL,
                    bc=smcSettings,
                )
                spline.SetName(node['name'] + " Stroke Geometry")
                strokeGeo.append(spline)
        strokeGeo = self.ConnectSplines(strokeGeo)

        # Create fill
        if len(node['fills']) > 0:
            fill = self.CreateFill(fillGeo, node['fills'], node['name'])
            fill.SetName(node['name'] + " Fill")
            fill.InsertUnder(group)

        # Create Stroke
        if len(node['strokes']) > 0:
            # alter geo for position of stroke if shape is closed
            if node['strokeAlign'] != "CENTER" and len(node['fillGeometry']) > 0:
                if node['strokeAlign'] == "INSIDE":
                    boolMode = c4d.SPLINEBOOL_MODE_AND
                else:
                    boolMode = c4d.SPLINEBOOL_MODE_AMINUSB

                trimmedStrokes = []

                # explode spline segments
                c4d.utils.SendModelingCommand(
                    command=c4d.MCOMMAND_EXPLODESEGMENTS,
                    list=[strokeGeo],
                )

                for seg in strokeGeo.GetChildren():
                    # filter out random lines
                    if seg.GetPointCount() > 2:
                        # perform bool
                        newSeg = c4d.utils.BooleanSplines(
                            seg,
                            [fillGeo],
                            self._C4DDOC,
                            self._C4DDOC.GetActiveBaseDraw(),
                            c4d.SPLINEBOOL_AXIS_XY,
                            boolMode
                        )
                        trimmedStrokes.append(newSeg)
                
                strokeGeo = self.ConnectSplines(trimmedStrokes)

            # extrude stroke
            stroke = self.CreateFill(strokeGeo, node['strokes'], node['name'])
            stroke.SetName(node['name'] + " Stroke")
            stroke.SetRelPos(c4d.Vector(0, 0, -self._DEPTH))
            stroke.InsertUnder(group)

        return group

    def CreateText(self, node):
        group = self.CreateNodeGroup(node)

        req = requests.get(
            'https://api.figma.com/v1/images/qS1uVPR59Z4UncGV8FUZZx', 
            headers={'X-Figma-Token': '41309-664fab20-e614-465b-bf81-ef2c463a5f2a'},
            params={
                'ids': node['id'], 
                'format': 'svg',
                'use_absolute_bounds': 'true',
                'svg_include_id': 'true',
            }
        )
        resJson = req.json()
        fileUrl = resJson['images'][node['id']]
        req = requests.get(fileUrl, allow_redirects=True)

        doc = minidom.parseString(req.content)  # parseString also exists
        paths = [path.getAttribute('d') for path in doc.getElementsByTagName('path')]
        doc.unlink()

        smcSettings = c4d.BaseContainer()
        smcSettings[c4d.MDATA_OPTIMIZE_TOLERANCE] = 0.001
        smcSettings[c4d.MDATA_OPTIMIZE_POINTS] = True
        smcSettings[c4d.MDATA_OPTIMIZE_POLYGONS] = False
        smcSettings[c4d.MDATA_OPTIMIZE_UNUSEDPOINTS] = True

        for path in paths:
            spline = self.SplineFromPath(path)
            c4d.utils.SendModelingCommand(
                command=c4d.MCOMMAND_OPTIMIZE,
                list=[spline],
                mode=c4d.MODELINGCOMMANDMODE_ALL,
                bc=smcSettings,
            )
            spline.InsertUnder(group)

        # # clipmap for font info
        # clipMap = c4d.bitmaps.GeClipMap()
        # fontSettings = clipMap.GetFontDescription(style['fontPostScriptName'], c4d.GE_FONT_NAME_POSTSCRIPT)
        # clipMap.Init(1, 1, 32)
        # clipMap.BeginDraw()
        # clipMap.SetFont(fontSettings, float(style['fontSize']))
        # # c4dTextHeight = clipMap.TextHeight()
        # clipMap.EndDraw()
        # clipMap.Destroy()

        # text.SetName(node['name'])
        
        # text[c4d.PRIM_TEXT_TEXT] = str(node['characters'])

        # fontData = c4d.FontData()
        # fontData.SetFont(fontSettings)
        # text[c4d.PRIM_TEXT_FONT] = fontData

        # alignSwitcher = {
        #     'LEFT': 0,
        #     'RIGHT': 2,
        #     'CENTER': 1,
        #     'JUSTIFIED': 1,
        # }
        # text[c4d.PRIM_TEXT_ALIGN] = alignSwitcher[style['textAlignHorizontal']]

        # text[c4d.PRIM_TEXT_HEIGHT] = style['fontSize']

        # text[c4d.PRIM_TEXT_HSPACING] = style['letterSpacing']

        # figmaLineHeightDifference = float(style['lineHeightPx']) - c4dTextHeight
        # text[c4d.PRIM_TEXT_VSPACING] = figmaLineHeightDifference

        # nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        # nodePos.y -= float(style['fontSize']) + (float(style['lineHeightPx']) - float(style['fontSize']))
        # text.SetAbsPos(nodePos)

        return group

    def CreateFrame(self, node):
        group = self.CreateNodeGroup(node)
        absBBox = node['absoluteBoundingBox']

        # layer bounds
        rect = c4d.BaseObject(c4d.Osplinerectangle)
        rect.SetName(node['name'] + " Bounding Box")
        rect[c4d.PRIM_RECTANGLE_WIDTH] = float(absBBox['width'])
        rect[c4d.PRIM_RECTANGLE_HEIGHT] = float(absBBox['height'])
        
        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        nodePos.x += absBBox['width'] / 2
        nodePos.y -= absBBox['height'] / 2

        # fill
        if len(node['fills']) > 0:
            fill = self.CreateFill(rect, node['fills'], node['name'])
            fill.SetName(node['name'] + " Fill")
            fill.SetAbsPos(nodePos)
            self.InsertLayerUnder(fill, group)
        
        # stroke
        if len(node['strokes']) > 0:
            stroke = self.CreateStroke(rect, node['strokes'], node['strokeWeight'], node['strokeAlign'], node['name'])
            stroke.SetName(node['name'] + " Stroke")
            stroke.SetAbsPos(c4d.Vector(nodePos.x, nodePos.y, -self._DEPTH))
            self.InsertLayerUnder(stroke, group)
        
        # spline if no fill or stroke
        if len(node['fills']) == 0 and len(node['strokes']) == 0:
            rect.SetAbsPos(nodePos)
            self.InsertLayerUnder(rect, group)

        return group

    def FigmaNodeToC4D(self, node):
        figmaType = node['type']

        switcher = {
            'FRAME': self.CreateFrame,
            'GROUP': self.CreateFrame,
            'COMPONENT': self.CreateFrame,
            'INSTANCE': self.CreateFrame,
            'SLICE': self.CreateFrame, # not sure about this
            'RECTANGLE': self.CreateVector,
            'REGULAR_POLYGON': self.CreateVector,
            'ELLIPSE': self.CreateVector,
            'LINE': self.CreateVector,
            'STAR': self.CreateVector,
            'VECTOR': self.CreateVector,
            'BOOLEAN_OPERATION': self.CreateVector,
            'TEXT': self.CreateText,
        }

        return switcher.get(figmaType, self.CreateFrame)(node)

    def RecurseFigmaDoc(self, node):
        nodes = []

        for layer in node:
            node = self.FigmaNodeToC4D(layer)

            if 'children' in layer.keys():
                if len(layer['children']) > 0:
                    childrenNodes = self.RecurseFigmaDoc(layer['children'])

                    for childNode in childrenNodes:
                            self.InsertLayerUnder(childNode, node)

            if node:
                nodes.append(node)

        return nodes


class DLG_IDS:
    BUTTON_GROUP = 1000
    EXECUTE = 1001

DLG_IDS = DLG_IDS()

class Fig4DDialog(c4d.gui.GeDialog):
    def CreateLayout(self):
        # Defines the title
        self.SetTitle("Fig4D")

        # Creates buttons
        if self.GroupBegin(id=DLG_IDS.BUTTON_GROUP, flags=c4d.BFH_SCALEFIT, rows=1, title="", cols=2, groupflags=0):
            self.AddButton(id=DLG_IDS.EXECUTE, flags=c4d.BFH_LEFT, initw=100, inith=25, name="Execute")
        self.GroupEnd()

        return True

    def Command(self, id, msg):
        if id == DLG_IDS.EXECUTE:
            pass
        
        return True

class Fig4DCommand(c4d.plugins.CommandData):
    dialog = None

    def Execute(self, doc):
        # Creates the dialog if its not already exists
        # if self.dialog is None:
        #     self.dialog = Figma4DDialog()

        # Opens the dialog
        # return self.dialog.Open(dlgtype=c4d.DLG_TYPE_ASYNC, pluginid=PLUGIN_ID, defaultw=250, defaulth=50)

        helper = FigmaHelper()
        helper._C4DDOC = c4d.documents.GetActiveDocument()
        helper._FILE = 'qS1uVPR59Z4UncGV8FUZZx'
        helper._TOKEN = '41309-664fab20-e614-465b-bf81-ef2c463a5f2a'

        return helper.SyncDoc()

    # def RestoreLayout(self, sec_ref):
    #     # Creates the dialog if its not already exists
    #     if self.dialog is None:
    #         self.dialog = Figma4DDialog()

    #     # Restores the layout
    #     return self.dialog.Restore(pluginid=PLUGIN_ID, secret=sec_ref)

if __name__ == "__main__":
    c4d.plugins.RegisterCommandPlugin(
        id=PLUGIN_ID, 
        str="Fig4D", 
        help="Import Figma files into C4D", 
        info=0, 
        dat=Fig4DCommand(),
        icon=None
        )

import c4d #pylint: disable=import-error
import json
import os
import re
import requests #pylint: disable=import-error

PLUGIN_ID = 1054883

class FigmaHelper():    
    def FetchFigmaDoc(self, file, token):
        req = requests.get(
            file, 
            headers={'X-Figma-Token': token},
            params={'geometry': 'paths' }
        )
        return req.json()

    def InsertLayerUnder(self, layer, parent):
        pos = layer.GetAbsPos()
        pos -= parent.GetAbsPos()

        layer.InsertUnder(parent)
        layer.SetAbsPos(pos)

    def PointsFromPath(self, path):
        closed = False
        points = []

        commands = re.findall(r'[A-Za-z][0-9\.\s]*', str(path))
        for command in commands:
            cType = command[0]
            coords = re.split(r"\s*", command[1:])
            if cType == "M":
                p = c4d.Vector(float(coords[0]), -float(coords[1]), 0.0)
                points.append(p)
            if cType == "L":
                p = c4d.Vector(float(coords[0]), -float(coords[1]), 0.0)
                points.append(p)
            if cType == "C":
                p = c4d.Vector(float(coords[4]), -float(coords[5]), 0.0)
                points.append(p)
            if cType == "Z":
                closed = True
            
        return {'count': len(points), 'closed': closed, 'points': points}

    def CreateVector(self, node):
        connect = c4d.BaseObject(c4d.Oconnector)
        absBBox = node['absoluteBoundingBox']

        connect.SetName(node['name'])
        
        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        connect.SetAbsPos(nodePos)

        for x in node['fillGeometry']:
            segStrings = re.findall(r"[Mm][^Mm]*", str(x['path']))
            for segString in segStrings:
                seg = self.PointsFromPath(segString)
                
                spline = c4d.SplineObject(0, c4d.SPLINETYPE_BEZIER)
                spline.ResizeObject(seg['count'])
                spline[c4d.SPLINEOBJECT_CLOSED] = seg['closed']
                for y in range(seg['count']):
                    p = seg['points'][y]
                    spline.SetPoint(y, p)
                spline.Message(c4d.MSG_UPDATE)
                spline.InsertUnder(connect)
            
        if 'visible' in node.keys():
            spline.SetEditorMode(c4d.MODE_OFF)
            spline.SetRenderMode(c4d.MODE_OFF)

        return connect

    def CreateRectangle(self, node):
        box = c4d.BaseObject(c4d.Osplinerectangle)
        absBBox = node['absoluteBoundingBox']

        box.SetName(node['name'])
        
        nodeSize = c4d.Vector(absBBox['width'], absBBox['height'], 10.0)
        box[c4d.PRIM_RECTANGLE_WIDTH] = nodeSize.x
        box[c4d.PRIM_RECTANGLE_HEIGHT] = nodeSize.y
        
        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        nodePos.x += absBBox['width'] / 2
        nodePos.y -= absBBox['height'] / 2
        box.SetAbsPos(nodePos)

        if 'visible' in node.keys():
            box.SetEditorMode(c4d.MODE_OFF)
            box.SetRenderMode(c4d.MODE_OFF)

        return box

    def CreateText(self, node):
        text = c4d.BaseObject(c4d.Osplinetext)
        absBBox = node['absoluteBoundingBox']
        style = node['style']

        # clipmap for font info
        clipMap = c4d.bitmaps.GeClipMap()
        fontSettings = clipMap.GetFontDescription(style['fontPostScriptName'], c4d.GE_FONT_NAME_POSTSCRIPT)
        clipMap.Init(1, 1, 32)
        clipMap.BeginDraw()
        clipMap.SetFont(fontSettings, float(style['fontSize']))
        # c4dTextHeight = clipMap.TextHeight()
        clipMap.EndDraw()
        clipMap.Destroy()

        text.SetName(node['name'])
        
        text[c4d.PRIM_TEXT_TEXT] = str(node['characters'])

        fontData = c4d.FontData()
        fontData.SetFont(fontSettings)
        text[c4d.PRIM_TEXT_FONT] = fontData

        alignSwitcher = {
            'LEFT': 0,
            'RIGHT': 2,
            'CENTER': 1,
            'JUSTIFIED': 1,
        }
        text[c4d.PRIM_TEXT_ALIGN] = alignSwitcher[style['textAlignHorizontal']]

        text[c4d.PRIM_TEXT_HEIGHT] = style['fontSize']

        text[c4d.PRIM_TEXT_HSPACING] = style['letterSpacing']

        # figmaLineHeightDifference = float(style['lineHeightPx']) - c4dTextHeight
        # text[c4d.PRIM_TEXT_VSPACING] = figmaLineHeightDifference

        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        nodePos.y -= float(style['fontSize']) + (float(style['lineHeightPx']) - float(style['fontSize']))
        text.SetAbsPos(nodePos)

        frame = self.CreateFrame(node)
        self.InsertLayerUnder(text, frame)

        return frame

    def CreateFrame(self, node):
        null = c4d.BaseObject(c4d.Onull)
        absBBox = node['absoluteBoundingBox']

        null.SetName(node['name'])

        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        nodePos.x += absBBox['width'] / 2
        nodePos.y -= absBBox['height'] / 2
        null.SetAbsPos(nodePos)

        if 'visible' in node.keys():
            null.SetEditorMode(c4d.MODE_OFF)
            null.SetRenderMode(c4d.MODE_OFF)
            del node['visible']

        rect = self.CreateRectangle(node)
        rect.SetName('Frame Bounding Box')
        self.InsertLayerUnder(rect, null)

        return null

    def FigmaNodeToC4D(self, node):
        figmaType = node['type']

        switcher = {
            'FRAME': self.CreateFrame,
            'GROUP': self.CreateFrame,
            'COMPONENT': self.CreateFrame,
            'INSTANCE': self.CreateFrame,
            'SLICE': self.CreateFrame, # not sure about this
            'RECTANGLE': self.CreateRectangle,
            'TEXT': self.CreateText,
            'REGULAR_POLYGON': self.CreateRectangle,
            'ELLIPSE': self.CreateRectangle,
            'LINE': self.CreateRectangle,
            'STAR': self.CreateRectangle,
            'BOOLEAN_OPERATION': self.CreateRectangle,
            'VECTOR': self.CreateVector,
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

class Figma4DDialog(c4d.gui.GeDialog):
    def CreateLayout(self):
        # Defines the title
        self.SetTitle("Figma4D")

        # Creates buttons
        if self.GroupBegin(id=DLG_IDS.BUTTON_GROUP, flags=c4d.BFH_SCALEFIT, rows=1, title="", cols=2, groupflags=0):
            self.AddButton(id=DLG_IDS.EXECUTE, flags=c4d.BFH_LEFT, initw=100, inith=25, name="Execute")
        self.GroupEnd()

        return True

    def Command(self, id, msg):
        if id == DLG_IDS.EXECUTE:
            pass
        
        return True

class Figma4DCommand(c4d.plugins.CommandData):
    dialog = None

    def Execute(self, doc):
        # Creates the dialog if its not already exists
        # if self.dialog is None:
        #     self.dialog = Figma4DDialog()

        # Opens the dialog
        # return self.dialog.Open(dlgtype=c4d.DLG_TYPE_ASYNC, pluginid=PLUGIN_ID, defaultw=250, defaulth=50)

        helper = FigmaHelper()
        uxFile = helper.FetchFigmaDoc('https://api.figma.com/v1/files/qS1uVPR59Z4UncGV8FUZZx', '41309-664fab20-e614-465b-bf81-ef2c463a5f2a')
        doc = c4d.documents.GetActiveDocument()
        frames = helper.RecurseFigmaDoc(uxFile['document']['children'][0]['children'])
        for frame in reversed(frames):
            doc.InsertObject(frame)

        c4d.EventAdd()

        return True

    # def RestoreLayout(self, sec_ref):
    #     # Creates the dialog if its not already exists
    #     if self.dialog is None:
    #         self.dialog = Figma4DDialog()

    #     # Restores the layout
    #     return self.dialog.Restore(pluginid=PLUGIN_ID, secret=sec_ref)

if __name__ == "__main__":
    c4d.plugins.RegisterCommandPlugin(
        id=PLUGIN_ID, 
        str="Figma4D", 
        help="Import Figma files into C4D", 
        info=0, 
        dat=Figma4DCommand(),
        icon=None
        )

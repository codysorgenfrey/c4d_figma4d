import c4d #pylint: disable=import-error
import json
import os
import requests #pylint: disable=import-error

PLUGIN_ID = 1054883

class FigmaHelper():    
    def FetchFigmaDoc(self, file, token):
        req = requests.get(file, headers={'X-Figma-Token': token})
        return req.json()

    def InsertLayerUnder(self, layer, parent):
        pos = layer.GetAbsPos()
        pos -= parent.GetAbsPos()

        layer.InsertUnder(parent)
        layer.SetAbsPos(pos)

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

        return box

    def CreateText(self, node):
        text = c4d.BaseObject(c4d.Osplinetext)
        absBBox = node['absoluteBoundingBox']
        style = node['style']

        text.SetName(node['name'])
        
        text[c4d.PRIM_TEXT_TEXT] = str(node['characters'])

        fontData = c4d.FontData()
        bc = c4d.BaseContainer()
        bc.SetString(500, 'Arial')
        bc.SetString(501, '11')
        bc.SetInt32(502, 400)
        bc.SetInt32(503, 0)
        bc.SetString(509, 'Arial')
        bc.SetString(508, 'ArialMT')
        fontData.SetFont(bc)
        text[c4d.PRIM_TEXT_FONT] = fontData

        return text

    def CreateFrame(self, node):
        null = c4d.BaseObject(c4d.Onull)
        absBBox = node['absoluteBoundingBox']

        null.SetName(node['name'])

        nodePos = c4d.Vector(absBBox['x'], -absBBox['y'], 0.0)
        nodePos.x += absBBox['width'] / 2
        nodePos.y -= absBBox['height'] / 2
        null.SetAbsPos(nodePos)

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
            'VECTOR': self.CreateRectangle,
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

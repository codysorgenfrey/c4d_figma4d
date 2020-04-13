import c4d #pylint: disable=import-error
import json
import os
import requests #pylint: disable=import-error

PLUGIN_ID = 1054883

class FigmaHelper():
    def RecurseFigmaDoc(self, node):
        for layer in node:
            print(layer['name'])
            if 'children' in layer.keys():
                if len(layer['children']) > 0:
                    self.RecurseFigmaDoc(layer['children'])
    
    def FetchFigmaDoc(self, file, token):
        req = requests.get(file, headers={'X-Figma-Token': token})
        return req.json()


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
        helper.RecurseFigmaDoc(uxFile['document']['children'])

        return True

    def RestoreLayout(self, sec_ref):
        # Creates the dialog if its not already exists
        # if self.dialog is None:
        #     self.dialog = Figma4DDialog()

        # Restores the layout
        # return self.dialog.Restore(pluginid=PLUGIN_ID, secret=sec_ref)
        return True

if __name__ == "__main__":
    c4d.plugins.RegisterCommandPlugin(
        id=PLUGIN_ID, 
        str="Figma4D", 
        help="Import Figma files into C4D", 
        info=0, 
        dat=Figma4DCommand(),
        icon=None
        )

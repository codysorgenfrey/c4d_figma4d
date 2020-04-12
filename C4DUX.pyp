import c4d #pylint: disable=import-error
import json
import os

PLUGIN_ID = 1054883

class DLG_IDS:
    BUTTON_GROUP = 1000
    EXECUTE = 1001

DLG_IDS = DLG_IDS()

class C4DUXDialog(c4d.gui.GeDialog):
    def CreateLayout(self):
        # Defines the title
        self.SetTitle("C4DUX")

        # Creates buttons
        if self.GroupBegin(id=DLG_IDS.BUTTON_GROUP, flags=c4d.BFH_SCALEFIT, rows=1, title="", cols=2, groupflags=0):
            self.AddButton(id=DLG_IDS.EXECUTE, flags=c4d.BFH_LEFT, initw=100, inith=25, name="Execute")
        self.GroupEnd()

        return True

    def Command(self, id, msg):
        if id == DLG_IDS.EXECUTE:
            with open('/Users/codysorgenfrey1/Downloads/test.json', 'r') as f:
                uxFile = json.load(data)
                for layer in uxFile:
                    print layer
        
        return True

class C4DUXCommand(c4d.plugins.CommandData):
    dialog = None

    def Execute(self, doc):
        # Creates the dialog if its not already exists
        if self.dialog is None:
            self.dialog = C4DUXDialog()

        # Opens the dialog
        return self.dialog.Open(dlgtype=c4d.DLG_TYPE_ASYNC, pluginid=PLUGIN_ID, defaultw=250, defaulth=50)

    def RestoreLayout(self, sec_ref):
        # Creates the dialog if its not already exists
        if self.dialog is None:
            self.dialog = TextureBakerDlg()

        # Restores the layout
        return self.dialog.Restore(pluginid=PLUGIN_ID, secret=sec_ref)

if __name__ == "__main__":
    c4d.plugins.RegisterCommandPlugin(
        id=PLUGIN_ID, 
        str="C4DUX", 
        help="Import Sketch and Figma files into C4D", 
        info=0, 
        dat=C4DUXCommand(),
        icon=None
        )

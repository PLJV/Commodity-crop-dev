from mainWindow import *
import sys,os

class platform:
    def __init__(self):
      self.hasWin32 = False
      self.hasMacos = False
      self.hasLinux = False
      # determine capability for win32 systems
      if sys.platform == "win32":
	self.hasWin32 = True
	try:
	  import ArcPy
	  self.hasArcPy = True
	except ImportError:
	  self.hasArcPy = False
	  QtGui.QMessageBox.information(None, "Documentation Warning.")
class MainWindow(QtGui.QMainWindow):
    def __init__(self):
	'''Derived subclass of QMainWindow; creates an instance of the MainWindow UI compiled by PyUIC and populates the window with controls.'''
        QtGui.QMainWindow.__init__(self)
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)
class Application():
    def __init__(self,Qwin,Qapp):
	'''Create instances of QApplication and QWindow, connect slots to Application methods, and sit on user input via _exec()'''
	# Define workspace input/output
	self.destinationFolder = None
	self.srcCountiesShapefile = None
	self.ssurgoPath = None
	# Define QApplication and QMainWindow connectors
	self.Qwin = Qwin
	self.Qapp = Qapp
	self._connectSlots()
	self.Qwin.show()
	self.Qapp.exec_()
    def _connectSlots(self):
	'''Connect QMainWindow objects to Application methods'''
	self.Qapp.connect(self.Qwin.ui.dstFolder_lineEdit, QtCore.SIGNAL("returnPressed()"), self.setDstDirectory)
	self.Qapp.connect(self.Qwin.ui.dstFldrOpen_pushButton, QtCore.SIGNAL("clicked()"), self.setDstDirectory)
	self.Qapp.connect(self.Qwin.ui.srcCounties_lineEdit, QtCore.SIGNAL("returnPressed()"), self.setSrcCounties)
	self.Qapp.connect(self.Qwin.ui.srcShpOpen_pushButton, QtCore.SIGNAL("clicked()"), self.setSrcCounties)
	self.Qapp.connect(self.Qwin.ui.ssurgo_lineEdit, QtCore.SIGNAL("returnPressed()"), self.setSsurgoPath)
	self.Qapp.connect(self.Qwin.ui.ssurgoOpen_pushButton, QtCore.SIGNAL("clicked()"), self.setSsurgoPath)
    def setDstDirectory(self):
	self.destinationFolder = QtGui.QFileDialog.getExistingDirectory(self.Qwin, 'Set destination directory')
	self.Qwin.ui.dstFolder_lineEdit.setText(QtCore.QString(self.destinationFolder))
    def setSrcCounties(self):
	self.srcCountiesShapefile = QtGui.QFileDialog.getOpenFileName(self.Qwin, 'Specify Shapefile containing target (county) boundaries')
	self.Qwin.ui.srcCounties_lineEdit.setText(QtCore.QString(self.srcCountiesShapefile))
    def setSsurgoPath(self):
	self.ssurgoPath = QtGui.QFileDialog.getOpenFileName(self.Qwin, 'Specify SSURGO state zipfile')
	self.Qwin.ui.ssurgo_lineEdit.setText(QtCore.QString(self.ssurgoPath))
    def isInputSane(self):
	print "not really"
if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    mainWindow = MainWindow()
    Application(mainWindow, app)
  
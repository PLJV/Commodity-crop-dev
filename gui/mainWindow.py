# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'mainWindow.ui'
#
# Created: Sat Jan  3 16:36:44 2015
#      by: PyQt4 UI code generator 4.10.4
#
# WARNING! All changes made in this file will be lost!

import sys

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.setObjectName(_fromUtf8("MainWindow"))
        MainWindow.resize(554, 221)
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(MainWindow.sizePolicy().hasHeightForWidth())
        MainWindow.setSizePolicy(sizePolicy)
        self.line = QtGui.QFrame(MainWindow)
        self.line.setGeometry(QtCore.QRect(0, 180, 531, 20))
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.tabWidget = QtGui.QTabWidget(MainWindow)
        self.tabWidget.setGeometry(QtCore.QRect(10, 10, 531, 141))
        self.tabWidget.setObjectName(_fromUtf8("tabWidget"))
        self.tab = QtGui.QWidget()
        self.tab.setObjectName(_fromUtf8("tab"))
        self.dstFldrOpen_pushButton = QtGui.QPushButton(self.tab)
        self.dstFldrOpen_pushButton.setGeometry(QtCore.QRect(440, 10, 75, 23))
        self.dstFldrOpen_pushButton.setObjectName(_fromUtf8("dstFldrOpen_pushButton"))
        self.label = QtGui.QLabel(self.tab)
        self.label.setGeometry(QtCore.QRect(10, 10, 71, 21))
        self.label.setObjectName(_fromUtf8("label"))
        self.dstFolder_lineEdit = QtGui.QLineEdit(self.tab)
        self.dstFolder_lineEdit.setGeometry(QtCore.QRect(90, 10, 341, 20))
        self.dstFolder_lineEdit.setObjectName(_fromUtf8("dstFolder_lineEdit"))
        self.tabWidget.addTab(self.tab, _fromUtf8(""))
        self.tab_2 = QtGui.QWidget()
        self.tab_2.setObjectName(_fromUtf8("tab_2"))
        self.srcCounties_lineEdit = QtGui.QLineEdit(self.tab_2)
        self.srcCounties_lineEdit.setGeometry(QtCore.QRect(90, 10, 321, 20))
        self.srcCounties_lineEdit.setObjectName(_fromUtf8("srcCounties_lineEdit"))
        self.label_2 = QtGui.QLabel(self.tab_2)
        self.label_2.setGeometry(QtCore.QRect(10, 10, 71, 21))
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.srcShpOpen_pushButton = QtGui.QPushButton(self.tab_2)
        self.srcShpOpen_pushButton.setGeometry(QtCore.QRect(430, 10, 75, 23))
        self.srcShpOpen_pushButton.setObjectName(_fromUtf8("srcShpOpen_pushButton"))
        self.ssurgoOpen_pushButton = QtGui.QPushButton(self.tab_2)
        self.ssurgoOpen_pushButton.setGeometry(QtCore.QRect(430, 40, 75, 23))
        self.ssurgoOpen_pushButton.setObjectName(_fromUtf8("ssurgoOpen_pushButton"))
        self.ssurgo_lineEdit = QtGui.QLineEdit(self.tab_2)
        self.ssurgo_lineEdit.setGeometry(QtCore.QRect(90, 40, 321, 20))
        self.ssurgo_lineEdit.setObjectName(_fromUtf8("ssurgo_lineEdit"))
        self.label_3 = QtGui.QLabel(self.tab_2)
        self.label_3.setGeometry(QtCore.QRect(10, 40, 71, 21))
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.tabWidget.addTab(self.tab_2, _fromUtf8(""))
        self.run_pushButton = QtGui.QPushButton(MainWindow)
        self.run_pushButton.setEnabled(False)
        self.run_pushButton.setGeometry(QtCore.QRect(460, 160, 81, 23))
        self.run_pushButton.setObjectName(_fromUtf8("run_pushButton"))
        self.progressBar = QtGui.QProgressBar(MainWindow)
        self.progressBar.setGeometry(QtCore.QRect(10, 160, 441, 23))
        self.progressBar.setProperty("value", 0)
        self.progressBar.setTextVisible(False)
        self.progressBar.setObjectName(_fromUtf8("progressBar"))
        self.status_lbl = QtGui.QLabel(MainWindow)
        self.status_lbl.setGeometry(QtCore.QRect(0, 200, 531, 16))
        self.status_lbl.setLayoutDirection(QtCore.Qt.RightToLeft)
        self.status_lbl.setAlignment(QtCore.Qt.AlignBottom|QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing)
        self.status_lbl.setObjectName(_fromUtf8("status_lbl"))

        self.retranslateUi(MainWindow)
        self.tabWidget.setCurrentIndex(1)
        QtCore.QMetaObject.connectSlotsByName(MainWindow)

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(_translate("MainWindow", "Tillage Likelihood Model", None))
        self.dstFldrOpen_pushButton.setText(_translate("MainWindow", "Open ...", None))
        self.label.setText(_translate("MainWindow", "Dest. Folder", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab), _translate("MainWindow", "Output", None))
        self.label_2.setText(_translate("MainWindow", "Counties", None))
        self.srcShpOpen_pushButton.setText(_translate("MainWindow", "Open ...", None))
        self.ssurgoOpen_pushButton.setText(_translate("MainWindow", "Open ...", None))
        self.label_3.setText(_translate("MainWindow", "SSURGO", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.tab_2), _translate("MainWindow", "Input", None))
        self.run_pushButton.setText(_translate("MainWindow", "run", None))
        self.status_lbl.setText(_translate("MainWindow", "Please specify run input/output data.", None))


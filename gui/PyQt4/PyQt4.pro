TEMPLATE = subdirs
CONFIG += ordered
SUBDIRS = QtCore QtGui QtHelp QtMultimedia QtNetwork QtDeclarative QtScript QtScriptTools QtXml QtOpenGL QtSql QtSvg QtTest QtWebKit QtXmlPatterns QtDesigner QtDBus Qt pylupdate pyrcc designer

init_py.files = /Users/ktaylora/PLJV/tillage_likelihood_model/gui/PyQt4/__init__.py
init_py.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += init_py

uic_package.files = /Users/ktaylora/PLJV/tillage_likelihood_model/gui/PyQt4/pyuic/uic
uic_package.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += uic_package

pyuic4.files = pyuic4
pyuic4.path = /System/Library/Frameworks/Python.framework/Versions/2.7/bin
INSTALLS += pyuic4

qscintilla_api.files = PyQt4.api
qscintilla_api.path = /opt/local/share/qt4/qsci/api/python
INSTALLS += qscintilla_api

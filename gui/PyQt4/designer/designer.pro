CONFIG      += plugin release warn_on

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += designer

    # Work around QTBUG-39300.
    CONFIG -= android_install
}

lessThan(QT_MAJOR_VERSION, 5) {
    CONFIG += designer
}

TARGET      = pyqt4
TEMPLATE    = lib

INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
LIBS        += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython2.7
DEFINES     += PYTHON_LIB=\\\"Python.framework/Versions/2.7/Python\\\"

SOURCES     = pluginloader.cpp
HEADERS     = pluginloader.h

# Install.
target.path = /opt/local/share/qt4/plugins/designer

python.path = /opt/local/share/qt4/plugins/designer
python.files = python

INSTALLS    += target python

INCLUDEPATH += /Users/ktaylora/PLJV/tillage_likelihood_model/gui/PyQt4/designer
VPATH = /Users/ktaylora/PLJV/tillage_likelihood_model/gui/PyQt4/designer


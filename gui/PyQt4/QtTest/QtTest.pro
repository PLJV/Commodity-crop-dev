TEMPLATE = lib
CONFIG += warn_on plugin
QT += testlib
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtTest.pyd
    target.files = QtTest.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtTest.so
    target.files = QtTest.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtTest
sip.files = ../sip/QtTest/qtestcase.sip ../sip/QtTest/qtestkeyboard.sip ../sip/QtTest/qtestmouse.sip ../sip/QtTest/qtestsystem.sip ../sip/QtTest/QtTestmod.sip
INSTALLS += sip
QMAKE_CXXFLAGS += -fno-exceptions
DEFINES += SIP_PROTECTED_IS_PUBLIC protected=public
INCLUDEPATH += .
INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7

win32 {
    QMAKE_POST_LINK = $(COPY_FILE) $(DESTDIR_TARGET) $$PY_MODULE
} else {
    QMAKE_POST_LINK = $(COPY_FILE) $(TARGET) $$PY_MODULE
}
macx {
    QMAKE_LFLAGS += "-undefined dynamic_lookup"
}

TARGET = QtTest
HEADERS = sipAPIQtTest.h
SOURCES = sipQtTestcmodule.cpp sipQtTestQTest.cpp

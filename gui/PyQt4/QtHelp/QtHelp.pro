TEMPLATE = lib
CONFIG += warn_on plugin
CONFIG += help
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtHelp.pyd
    target.files = QtHelp.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtHelp.so
    target.files = QtHelp.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtHelp
sip.files = ../sip/QtHelp/qhelpcontentwidget.sip ../sip/QtHelp/qhelpengine.sip ../sip/QtHelp/qhelpenginecore.sip ../sip/QtHelp/qhelpindexwidget.sip ../sip/QtHelp/qhelpsearchengine.sip ../sip/QtHelp/qhelpsearchquerywidget.sip ../sip/QtHelp/qhelpsearchresultwidget.sip ../sip/QtHelp/QtHelpmod.sip
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

TARGET = QtHelp
HEADERS = sipAPIQtHelp.h
SOURCES = sipQtHelpcmodule.cpp sipQtHelpQHelpContentItem.cpp sipQtHelpQHelpContentModel.cpp sipQtHelpQHelpContentWidget.cpp sipQtHelpQHelpEngine.cpp sipQtHelpQHelpEngineCore.cpp sipQtHelpQHelpIndexModel.cpp sipQtHelpQHelpIndexWidget.cpp sipQtHelpQHelpSearchEngine.cpp sipQtHelpQHelpSearchQuery.cpp sipQtHelpQHelpSearchQueryWidget.cpp sipQtHelpQHelpSearchResultWidget.cpp sipQtHelpQList0100QHelpSearchQuery.cpp sipQtHelpQList0100QStringList.cpp sipQtHelpQMap0100QString0100QUrl.cpp

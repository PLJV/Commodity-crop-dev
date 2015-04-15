TEMPLATE = lib
CONFIG += warn_on plugin
QT += webkit network
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtWebKit.pyd
    target.files = QtWebKit.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtWebKit.so
    target.files = QtWebKit.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtWebKit
sip.files = ../sip/QtWebKit/qgraphicswebview.sip ../sip/QtWebKit/QtWebKitmod.sip ../sip/QtWebKit/qwebdatabase.sip ../sip/QtWebKit/qwebelement.sip ../sip/QtWebKit/qwebframe.sip ../sip/QtWebKit/qwebhistory.sip ../sip/QtWebKit/qwebhistoryinterface.sip ../sip/QtWebKit/qwebinspector.sip ../sip/QtWebKit/qwebkitglobal.sip ../sip/QtWebKit/qwebkitversion.sip ../sip/QtWebKit/qwebpage.sip ../sip/QtWebKit/qwebpluginfactory.sip ../sip/QtWebKit/qwebsecurityorigin.sip ../sip/QtWebKit/qwebsettings.sip ../sip/QtWebKit/qwebview.sip
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

TARGET = QtWebKit
HEADERS = sipAPIQtWebKit.h
SOURCES = sipQtWebKitcmodule.cpp sipQtWebKitQGraphicsWebView.cpp sipQtWebKitQList0100QWebDatabase.cpp sipQtWebKitQList0100QWebElement.cpp sipQtWebKitQList0100QWebHistoryItem.cpp sipQtWebKitQList0100QWebPluginFactoryMimeType.cpp sipQtWebKitQList0100QWebPluginFactoryPlugin.cpp sipQtWebKitQList0100QWebSecurityOrigin.cpp sipQtWebKitQList0101QWebFrame.cpp sipQtWebKitQMultiMap0100QString0100QString.cpp sipQtWebKitQWebDatabase.cpp sipQtWebKitQWebElement.cpp sipQtWebKitQWebElementCollection.cpp sipQtWebKitQWebFrame.cpp sipQtWebKitQWebHistory.cpp sipQtWebKitQWebHistoryInterface.cpp sipQtWebKitQWebHistoryItem.cpp sipQtWebKitQWebHitTestResult.cpp sipQtWebKitQWebInspector.cpp sipQtWebKitQWebPage.cpp sipQtWebKitQWebPageChooseMultipleFilesExtensionOption.cpp sipQtWebKitQWebPageChooseMultipleFilesExtensionReturn.cpp sipQtWebKitQWebPageErrorPageExtensionOption.cpp sipQtWebKitQWebPageErrorPageExtensionReturn.cpp sipQtWebKitQWebPageExtensionOption.cpp sipQtWebKitQWebPageExtensionReturn.cpp sipQtWebKitQWebPageFindFlags.cpp sipQtWebKitQWebPageViewportAttributes.cpp sipQtWebKitQWebPluginFactory.cpp sipQtWebKitQWebPluginFactoryExtensionOption.cpp sipQtWebKitQWebPluginFactoryExtensionReturn.cpp sipQtWebKitQWebPluginFactoryMimeType.cpp sipQtWebKitQWebPluginFactoryPlugin.cpp sipQtWebKitQWebSecurityOrigin.cpp sipQtWebKitQWebSettings.cpp sipQtWebKitQWebView.cpp

TEMPLATE = lib
CONFIG += warn_on plugin
QT += declarative network
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtDeclarative.pyd
    target.files = QtDeclarative.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtDeclarative.so
    target.files = QtDeclarative.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtDeclarative
sip.files = ../sip/QtDeclarative/qdeclarativecomponent.sip ../sip/QtDeclarative/qdeclarativecontext.sip ../sip/QtDeclarative/qdeclarativeengine.sip ../sip/QtDeclarative/qdeclarativeerror.sip ../sip/QtDeclarative/qdeclarativeexpression.sip ../sip/QtDeclarative/qdeclarativeextensionplugin.sip ../sip/QtDeclarative/qdeclarativeimageprovider.sip ../sip/QtDeclarative/qdeclarativeitem.sip ../sip/QtDeclarative/qdeclarativelist.sip ../sip/QtDeclarative/qdeclarativenetworkaccessmanagerfactory.sip ../sip/QtDeclarative/qdeclarativeparserstatus.sip ../sip/QtDeclarative/qdeclarativeproperty.sip ../sip/QtDeclarative/qdeclarativepropertymap.sip ../sip/QtDeclarative/qdeclarativepropertyvaluesource.sip ../sip/QtDeclarative/qdeclarativescriptstring.sip ../sip/QtDeclarative/qdeclarativeview.sip ../sip/QtDeclarative/qpydeclarativelistproperty.sip ../sip/QtDeclarative/qpydeclarativepropertyvaluesource.sip ../sip/QtDeclarative/QtDeclarativemod.sip
INSTALLS += sip
QMAKE_CXXFLAGS += -fno-exceptions
DEFINES += SIP_PROTECTED_IS_PUBLIC protected=public
INCLUDEPATH += .
INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
INCLUDEPATH += ../qpy/QtDeclarative

win32 {
    QMAKE_POST_LINK = $(COPY_FILE) $(DESTDIR_TARGET) $$PY_MODULE
} else {
    QMAKE_POST_LINK = $(COPY_FILE) $(TARGET) $$PY_MODULE
}
macx {
    QMAKE_LFLAGS += "-undefined dynamic_lookup"
}

TARGET = QtDeclarative
HEADERS = sipAPIQtDeclarative.h ../qpy/QtDeclarative/qpydeclarative_api.h ../qpy/QtDeclarative/qpydeclarative_chimera_helpers.h ../qpy/QtDeclarative/qpydeclarative_listwrapper.h ../qpy/QtDeclarative/qpydeclarativelistproperty.h ../qpy/QtDeclarative/qpydeclarativepropertyvaluesource.h
SOURCES = sipQtDeclarativecmodule.cpp sipQtDeclarativeQDeclarativeComponent.cpp sipQtDeclarativeQDeclarativeContext.cpp sipQtDeclarativeQDeclarativeEngine.cpp sipQtDeclarativeQDeclarativeError.cpp sipQtDeclarativeQDeclarativeExpression.cpp sipQtDeclarativeQDeclarativeExtensionPlugin.cpp sipQtDeclarativeQDeclarativeImageProvider.cpp sipQtDeclarativeQDeclarativeItem.cpp sipQtDeclarativeQDeclarativeListProperty0100QObject.cpp sipQtDeclarativeQDeclarativeListReference.cpp sipQtDeclarativeQDeclarativeNetworkAccessManagerFactory.cpp sipQtDeclarativeQDeclarativeParserStatus.cpp sipQtDeclarativeQDeclarativeProperty.cpp sipQtDeclarativeQDeclarativePropertyMap.cpp sipQtDeclarativeQDeclarativePropertyValueSource.cpp sipQtDeclarativeQDeclarativeScriptString.cpp sipQtDeclarativeQDeclarativeView.cpp sipQtDeclarativeQList0100QDeclarativeError.cpp sipQtDeclarativeQPyDeclarativePropertyValueSource.cpp ../qpy/QtDeclarative/qpydeclarative_chimera_helpers.cpp ../qpy/QtDeclarative/qpydeclarative_listwrapper.cpp ../qpy/QtDeclarative/qpydeclarative_post_init.cpp ../qpy/QtDeclarative/qpydeclarativelistproperty.cpp

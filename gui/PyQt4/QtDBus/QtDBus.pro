TEMPLATE = lib
CONFIG += warn_on plugin
QT -= gui
QT += dbus
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtDBus.pyd
    target.files = QtDBus.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtDBus.so
    target.files = QtDBus.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtDBus
sip.files = ../sip/QtDBus/qdbusabstractadaptor.sip ../sip/QtDBus/qdbusabstractinterface.sip ../sip/QtDBus/qdbusargument.sip ../sip/QtDBus/qdbusconnection.sip ../sip/QtDBus/qdbusconnectioninterface.sip ../sip/QtDBus/qdbuserror.sip ../sip/QtDBus/qdbusextratypes.sip ../sip/QtDBus/qdbusinterface.sip ../sip/QtDBus/qdbusmessage.sip ../sip/QtDBus/qdbuspendingcall.sip ../sip/QtDBus/qdbusservicewatcher.sip ../sip/QtDBus/qdbusunixfiledescriptor.sip ../sip/QtDBus/qpydbuspendingreply.sip ../sip/QtDBus/qpydbusreply.sip ../sip/QtDBus/QtDBusmod.sip
INSTALLS += sip
QMAKE_CXXFLAGS += -fno-exceptions
DEFINES += SIP_PROTECTED_IS_PUBLIC protected=public
INCLUDEPATH += .
INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
INCLUDEPATH += ../qpy/QtDBus

win32 {
    QMAKE_POST_LINK = $(COPY_FILE) $(DESTDIR_TARGET) $$PY_MODULE
} else {
    QMAKE_POST_LINK = $(COPY_FILE) $(TARGET) $$PY_MODULE
}
macx {
    QMAKE_LFLAGS += "-undefined dynamic_lookup"
}

TARGET = QtDBus
HEADERS = sipAPIQtDBus.h ../qpy/QtDBus/qpydbus_api.h ../qpy/QtDBus/qpydbus_chimera_helpers.h ../qpy/QtDBus/qpydbuspendingreply.h ../qpy/QtDBus/qpydbusreply.h
SOURCES = sipQtDBuscmodule.cpp sipQtDBusQDBus.cpp sipQtDBusQDBusAbstractAdaptor.cpp sipQtDBusQDBusAbstractInterface.cpp sipQtDBusQDBusArgument.cpp sipQtDBusQDBusConnection.cpp sipQtDBusQDBusConnectionConnectionCapabilities.cpp sipQtDBusQDBusConnectionInterface.cpp sipQtDBusQDBusConnectionRegisterOptions.cpp sipQtDBusQDBusError.cpp sipQtDBusQDBusInterface.cpp sipQtDBusQDBusMessage.cpp sipQtDBusQDBusObjectPath.cpp sipQtDBusQDBusPendingCall.cpp sipQtDBusQDBusPendingCallWatcher.cpp sipQtDBusQDBusReply0100QDBusConnectionInterfaceRegisterServiceReply.cpp sipQtDBusQDBusReply0100QString.cpp sipQtDBusQDBusReply0100QStringList.cpp sipQtDBusQDBusReply0400.cpp sipQtDBusQDBusReply1900.cpp sipQtDBusQDBusReply2600.cpp sipQtDBusQDBusServiceWatcher.cpp sipQtDBusQDBusServiceWatcherWatchMode.cpp sipQtDBusQDBusSignature.cpp sipQtDBusQDBusUnixFileDescriptor.cpp sipQtDBusQDBusVariant.cpp sipQtDBusQPyDBusPendingReply.cpp sipQtDBusQPyDBusReply.cpp ../qpy/QtDBus/qpydbus_chimera_helpers.cpp ../qpy/QtDBus/qpydbus_post_init.cpp ../qpy/QtDBus/qpydbuspendingreply.cpp ../qpy/QtDBus/qpydbusreply.cpp

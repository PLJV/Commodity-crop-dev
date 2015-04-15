TEMPLATE = lib
CONFIG += warn_on plugin
QT -= gui
QT += network
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtNetwork.pyd
    target.files = QtNetwork.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtNetwork.so
    target.files = QtNetwork.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtNetwork
sip.files = ../sip/QtNetwork/qabstractnetworkcache.sip ../sip/QtNetwork/qabstractsocket.sip ../sip/QtNetwork/qauthenticator.sip ../sip/QtNetwork/qdnslookup.sip ../sip/QtNetwork/qftp.sip ../sip/QtNetwork/qhostaddress.sip ../sip/QtNetwork/qhostinfo.sip ../sip/QtNetwork/qhttp.sip ../sip/QtNetwork/qhttpmultipart.sip ../sip/QtNetwork/qlocalserver.sip ../sip/QtNetwork/qlocalsocket.sip ../sip/QtNetwork/qnetworkaccessmanager.sip ../sip/QtNetwork/qnetworkconfigmanager.sip ../sip/QtNetwork/qnetworkconfiguration.sip ../sip/QtNetwork/qnetworkcookie.sip ../sip/QtNetwork/qnetworkcookiejar.sip ../sip/QtNetwork/qnetworkdiskcache.sip ../sip/QtNetwork/qnetworkinterface.sip ../sip/QtNetwork/qnetworkproxy.sip ../sip/QtNetwork/qnetworkreply.sip ../sip/QtNetwork/qnetworkrequest.sip ../sip/QtNetwork/qnetworksession.sip ../sip/QtNetwork/qssl.sip ../sip/QtNetwork/qsslcertificate.sip ../sip/QtNetwork/qsslcertificateextension.sip ../sip/QtNetwork/qsslcipher.sip ../sip/QtNetwork/qsslconfiguration.sip ../sip/QtNetwork/qsslerror.sip ../sip/QtNetwork/qsslkey.sip ../sip/QtNetwork/qsslsocket.sip ../sip/QtNetwork/qtcpserver.sip ../sip/QtNetwork/qtcpsocket.sip ../sip/QtNetwork/QtNetworkmod.sip ../sip/QtNetwork/qudpsocket.sip ../sip/QtNetwork/qurlinfo.sip
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

TARGET = QtNetwork
HEADERS = sipAPIQtNetwork.h
SOURCES = sipQtNetworkcmodule.cpp sipQtNetworkQ_IPV6ADDR.cpp sipQtNetworkQAbstractNetworkCache.cpp sipQtNetworkQAbstractSocket.cpp sipQtNetworkQAuthenticator.cpp sipQtNetworkQFtp.cpp sipQtNetworkQHash0100QNetworkRequestAttribute0100QVariant.cpp sipQtNetworkQHostAddress.cpp sipQtNetworkQHostInfo.cpp sipQtNetworkQHttp.cpp sipQtNetworkQHttpHeader.cpp sipQtNetworkQHttpMultiPart.cpp sipQtNetworkQHttpPart.cpp sipQtNetworkQHttpRequestHeader.cpp sipQtNetworkQHttpResponseHeader.cpp sipQtNetworkQList0100QHostAddress.cpp sipQtNetworkQList0100QNetworkAddressEntry.cpp sipQtNetworkQList0100QNetworkConfiguration.cpp sipQtNetworkQList0100QNetworkCookie.cpp sipQtNetworkQList0100QNetworkInterface.cpp sipQtNetworkQList0100QNetworkProxy.cpp sipQtNetworkQList0100QSslCertificate.cpp sipQtNetworkQList0100QSslCipher.cpp sipQtNetworkQList0100QSslError.cpp sipQtNetworkQLocalServer.cpp sipQtNetworkQLocalSocket.cpp sipQtNetworkQMultiMap0100QSslAlternateNameEntryType0100QString.cpp sipQtNetworkQNetworkAccessManager.cpp sipQtNetworkQNetworkAddressEntry.cpp sipQtNetworkQNetworkCacheMetaData.cpp sipQtNetworkQNetworkConfiguration.cpp sipQtNetworkQNetworkConfigurationManager.cpp sipQtNetworkQNetworkConfigurationManagerCapabilities.cpp sipQtNetworkQNetworkConfigurationStateFlags.cpp sipQtNetworkQNetworkCookie.cpp sipQtNetworkQNetworkCookieJar.cpp sipQtNetworkQNetworkDiskCache.cpp sipQtNetworkQNetworkInterface.cpp sipQtNetworkQNetworkInterfaceInterfaceFlags.cpp sipQtNetworkQNetworkProxy.cpp sipQtNetworkQNetworkProxyCapabilities.cpp sipQtNetworkQNetworkProxyFactory.cpp sipQtNetworkQNetworkProxyQuery.cpp sipQtNetworkQNetworkReply.cpp sipQtNetworkQNetworkRequest.cpp sipQtNetworkQNetworkSession.cpp sipQtNetworkQPair0100QHostAddress1800.cpp sipQtNetworkQSsl.cpp sipQtNetworkQSslCertificate.cpp sipQtNetworkQSslCipher.cpp sipQtNetworkQSslConfiguration.cpp sipQtNetworkQSslError.cpp sipQtNetworkQSslKey.cpp sipQtNetworkQSslSocket.cpp sipQtNetworkQSslSslOptions.cpp sipQtNetworkQTcpServer.cpp sipQtNetworkQTcpSocket.cpp sipQtNetworkQUdpSocket.cpp sipQtNetworkQUdpSocketBindMode.cpp sipQtNetworkQUrlInfo.cpp

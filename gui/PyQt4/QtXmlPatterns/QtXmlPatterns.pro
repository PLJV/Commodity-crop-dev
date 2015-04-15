TEMPLATE = lib
CONFIG += warn_on plugin
QT -= gui
QT += xmlpatterns network
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtXmlPatterns.pyd
    target.files = QtXmlPatterns.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtXmlPatterns.so
    target.files = QtXmlPatterns.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtXmlPatterns
sip.files = ../sip/QtXmlPatterns/qabstractmessagehandler.sip ../sip/QtXmlPatterns/qabstracturiresolver.sip ../sip/QtXmlPatterns/qabstractxmlnodemodel.sip ../sip/QtXmlPatterns/qabstractxmlreceiver.sip ../sip/QtXmlPatterns/qsimplexmlnodemodel.sip ../sip/QtXmlPatterns/qsourcelocation.sip ../sip/QtXmlPatterns/QtXmlPatternsmod.sip ../sip/QtXmlPatterns/qxmlformatter.sip ../sip/QtXmlPatterns/qxmlname.sip ../sip/QtXmlPatterns/qxmlnamepool.sip ../sip/QtXmlPatterns/qxmlquery.sip ../sip/QtXmlPatterns/qxmlresultitems.sip ../sip/QtXmlPatterns/qxmlschema.sip ../sip/QtXmlPatterns/qxmlschemavalidator.sip ../sip/QtXmlPatterns/qxmlserializer.sip
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

TARGET = QtXmlPatterns
HEADERS = sipAPIQtXmlPatterns.h
SOURCES = sipQtXmlPatternscmodule.cpp sipQtXmlPatternsQAbstractMessageHandler.cpp sipQtXmlPatternsQAbstractUriResolver.cpp sipQtXmlPatternsQAbstractXmlNodeModel.cpp sipQtXmlPatternsQAbstractXmlReceiver.cpp sipQtXmlPatternsQSimpleXmlNodeModel.cpp sipQtXmlPatternsQSourceLocation.cpp sipQtXmlPatternsQVector0100QXmlName.cpp sipQtXmlPatternsQVector0100QXmlNodeModelIndex.cpp sipQtXmlPatternsQXmlFormatter.cpp sipQtXmlPatternsQXmlItem.cpp sipQtXmlPatternsQXmlName.cpp sipQtXmlPatternsQXmlNamePool.cpp sipQtXmlPatternsQXmlNodeModelIndex.cpp sipQtXmlPatternsQXmlQuery.cpp sipQtXmlPatternsQXmlResultItems.cpp sipQtXmlPatternsQXmlSchema.cpp sipQtXmlPatternsQXmlSchemaValidator.cpp sipQtXmlPatternsQXmlSerializer.cpp

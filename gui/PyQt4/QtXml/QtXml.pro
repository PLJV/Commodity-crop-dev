TEMPLATE = lib
CONFIG += warn_on plugin
QT -= gui
QT += xml
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtXml.pyd
    target.files = QtXml.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtXml.so
    target.files = QtXml.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtXml
sip.files = ../sip/QtXml/qdom.sip ../sip/QtXml/QtXmlmod.sip ../sip/QtXml/qxml.sip ../sip/QtXml/qxmlstream.sip
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

TARGET = QtXml
HEADERS = sipAPIQtXml.h
SOURCES = sipQtXmlcmodule.cpp sipQtXmlQDomAttr.cpp sipQtXmlQDomCDATASection.cpp sipQtXmlQDomCharacterData.cpp sipQtXmlQDomComment.cpp sipQtXmlQDomDocument.cpp sipQtXmlQDomDocumentFragment.cpp sipQtXmlQDomDocumentType.cpp sipQtXmlQDomElement.cpp sipQtXmlQDomEntity.cpp sipQtXmlQDomEntityReference.cpp sipQtXmlQDomImplementation.cpp sipQtXmlQDomNamedNodeMap.cpp sipQtXmlQDomNode.cpp sipQtXmlQDomNodeList.cpp sipQtXmlQDomNotation.cpp sipQtXmlQDomProcessingInstruction.cpp sipQtXmlQDomText.cpp sipQtXmlQXmlAttributes.cpp sipQtXmlQXmlContentHandler.cpp sipQtXmlQXmlDeclHandler.cpp sipQtXmlQXmlDefaultHandler.cpp sipQtXmlQXmlDTDHandler.cpp sipQtXmlQXmlEntityResolver.cpp sipQtXmlQXmlErrorHandler.cpp sipQtXmlQXmlInputSource.cpp sipQtXmlQXmlLexicalHandler.cpp sipQtXmlQXmlLocator.cpp sipQtXmlQXmlNamespaceSupport.cpp sipQtXmlQXmlParseException.cpp sipQtXmlQXmlReader.cpp sipQtXmlQXmlSimpleReader.cpp

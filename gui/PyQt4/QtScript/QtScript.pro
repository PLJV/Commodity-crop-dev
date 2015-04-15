TEMPLATE = lib
CONFIG += warn_on plugin
QT -= gui
QT += script
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtScript.pyd
    target.files = QtScript.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtScript.so
    target.files = QtScript.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtScript
sip.files = ../sip/QtScript/qscriptclass.sip ../sip/QtScript/qscriptclasspropertyiterator.sip ../sip/QtScript/qscriptcontext.sip ../sip/QtScript/qscriptcontextinfo.sip ../sip/QtScript/qscriptengine.sip ../sip/QtScript/qscriptengineagent.sip ../sip/QtScript/qscriptstring.sip ../sip/QtScript/qscriptvalue.sip ../sip/QtScript/qscriptvalueiterator.sip ../sip/QtScript/QtScriptmod.sip
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

TARGET = QtScript
HEADERS = sipAPIQtScript.h
SOURCES = sipQtScriptcmodule.cpp sipQtScriptQList0100QScriptContextInfo.cpp sipQtScriptQList0100QScriptValue.cpp sipQtScriptQScriptClass.cpp sipQtScriptQScriptClassPropertyIterator.cpp sipQtScriptQScriptClassQueryFlags.cpp sipQtScriptQScriptContext.cpp sipQtScriptQScriptContextInfo.cpp sipQtScriptQScriptEngine.cpp sipQtScriptQScriptEngineAgent.cpp sipQtScriptQScriptEngineQObjectWrapOptions.cpp sipQtScriptQScriptString.cpp sipQtScriptQScriptSyntaxCheckResult.cpp sipQtScriptQScriptValue.cpp sipQtScriptQScriptValueIterator.cpp sipQtScriptQScriptValuePropertyFlags.cpp sipQtScriptQScriptValueResolveFlags.cpp

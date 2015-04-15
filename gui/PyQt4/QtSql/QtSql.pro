TEMPLATE = lib
CONFIG += warn_on plugin
QT += sql
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtSql.pyd
    target.files = QtSql.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtSql.so
    target.files = QtSql.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtSql
sip.files = ../sip/QtSql/qsql.sip ../sip/QtSql/qsqldatabase.sip ../sip/QtSql/qsqldriver.sip ../sip/QtSql/qsqlerror.sip ../sip/QtSql/qsqlfield.sip ../sip/QtSql/qsqlindex.sip ../sip/QtSql/qsqlquery.sip ../sip/QtSql/qsqlquerymodel.sip ../sip/QtSql/qsqlrecord.sip ../sip/QtSql/qsqlrelationaldelegate.sip ../sip/QtSql/qsqlrelationaltablemodel.sip ../sip/QtSql/qsqlresult.sip ../sip/QtSql/qsqltablemodel.sip ../sip/QtSql/QtSqlmod.sip
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

TARGET = QtSql
HEADERS = sipAPIQtSql.h
SOURCES = sipQtSqlcmodule.cpp sipQtSqlQSql.cpp sipQtSqlQSqlDatabase.cpp sipQtSqlQSqlDriver.cpp sipQtSqlQSqlDriverCreatorBase.cpp sipQtSqlQSqlError.cpp sipQtSqlQSqlField.cpp sipQtSqlQSqlIndex.cpp sipQtSqlQSqlParamType.cpp sipQtSqlQSqlQuery.cpp sipQtSqlQSqlQueryModel.cpp sipQtSqlQSqlRecord.cpp sipQtSqlQSqlRelation.cpp sipQtSqlQSqlRelationalDelegate.cpp sipQtSqlQSqlRelationalTableModel.cpp sipQtSqlQSqlResult.cpp sipQtSqlQSqlTableModel.cpp sipQtSqlQVector0100QVariant.cpp

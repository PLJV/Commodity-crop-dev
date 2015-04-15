TEMPLATE = lib
CONFIG += warn_on plugin
CONFIG += designer
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtDesigner.pyd
    target.files = QtDesigner.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtDesigner.so
    target.files = QtDesigner.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtDesigner
sip.files = ../sip/QtDesigner/abstractactioneditor.sip ../sip/QtDesigner/abstractformbuilder.sip ../sip/QtDesigner/abstractformeditor.sip ../sip/QtDesigner/abstractformwindow.sip ../sip/QtDesigner/abstractformwindowcursor.sip ../sip/QtDesigner/abstractformwindowmanager.sip ../sip/QtDesigner/abstractobjectinspector.sip ../sip/QtDesigner/abstractpropertyeditor.sip ../sip/QtDesigner/abstractwidgetbox.sip ../sip/QtDesigner/container.sip ../sip/QtDesigner/customwidget.sip ../sip/QtDesigner/default_extensionfactory.sip ../sip/QtDesigner/extension.sip ../sip/QtDesigner/formbuilder.sip ../sip/QtDesigner/membersheet.sip ../sip/QtDesigner/propertysheet.sip ../sip/QtDesigner/qextensionmanager.sip ../sip/QtDesigner/qpydesignercontainerextension.sip ../sip/QtDesigner/qpydesignercustomwidgetcollectionplugin.sip ../sip/QtDesigner/qpydesignercustomwidgetplugin.sip ../sip/QtDesigner/qpydesignermembersheetextension.sip ../sip/QtDesigner/qpydesignerpropertysheetextension.sip ../sip/QtDesigner/qpydesignertaskmenuextension.sip ../sip/QtDesigner/QtDesignermod.sip ../sip/QtDesigner/taskmenu.sip
INSTALLS += sip
QMAKE_CXXFLAGS += -fno-exceptions
DEFINES += SIP_PROTECTED_IS_PUBLIC protected=public
INCLUDEPATH += .
INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
INCLUDEPATH += ../qpy/QtDesigner

win32 {
    QMAKE_POST_LINK = $(COPY_FILE) $(DESTDIR_TARGET) $$PY_MODULE
} else {
    QMAKE_POST_LINK = $(COPY_FILE) $(TARGET) $$PY_MODULE
}
macx {
    QMAKE_LFLAGS += "-undefined dynamic_lookup"
}

TARGET = QtDesigner
HEADERS = sipAPIQtDesigner.h ../qpy/QtDesigner/qpydesignercontainerextension.h ../qpy/QtDesigner/qpydesignercustomwidgetcollectionplugin.h ../qpy/QtDesigner/qpydesignercustomwidgetplugin.h ../qpy/QtDesigner/qpydesignermembersheetextension.h ../qpy/QtDesigner/qpydesignerpropertysheetextension.h ../qpy/QtDesigner/qpydesignertaskmenuextension.h
SOURCES = sipQtDesignercmodule.cpp sipQtDesignerQAbstractExtensionFactory.cpp sipQtDesignerQAbstractExtensionManager.cpp sipQtDesignerQAbstractFormBuilder.cpp sipQtDesignerQDesignerActionEditorInterface.cpp sipQtDesignerQDesignerContainerExtension.cpp sipQtDesignerQDesignerCustomWidgetCollectionInterface.cpp sipQtDesignerQDesignerCustomWidgetInterface.cpp sipQtDesignerQDesignerFormEditorInterface.cpp sipQtDesignerQDesignerFormWindowCursorInterface.cpp sipQtDesignerQDesignerFormWindowInterface.cpp sipQtDesignerQDesignerFormWindowInterfaceFeature.cpp sipQtDesignerQDesignerFormWindowManagerInterface.cpp sipQtDesignerQDesignerMemberSheetExtension.cpp sipQtDesignerQDesignerObjectInspectorInterface.cpp sipQtDesignerQDesignerPropertyEditorInterface.cpp sipQtDesignerQDesignerPropertySheetExtension.cpp sipQtDesignerQDesignerTaskMenuExtension.cpp sipQtDesignerQDesignerWidgetBoxInterface.cpp sipQtDesignerQExtensionFactory.cpp sipQtDesignerQExtensionManager.cpp sipQtDesignerQFormBuilder.cpp sipQtDesignerQList0101QDesignerCustomWidgetInterface.cpp sipQtDesignerQPyDesignerContainerExtension.cpp sipQtDesignerQPyDesignerCustomWidgetCollectionPlugin.cpp sipQtDesignerQPyDesignerCustomWidgetPlugin.cpp sipQtDesignerQPyDesignerMemberSheetExtension.cpp sipQtDesignerQPyDesignerPropertySheetExtension.cpp sipQtDesignerQPyDesignerTaskMenuExtension.cpp

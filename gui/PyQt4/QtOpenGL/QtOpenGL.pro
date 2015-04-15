TEMPLATE = lib
CONFIG += warn_on plugin
QT += opengl
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtOpenGL.pyd
    target.files = QtOpenGL.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtOpenGL.so
    target.files = QtOpenGL.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtOpenGL
sip.files = ../sip/QtOpenGL/qgl.sip ../sip/QtOpenGL/qglbuffer.sip ../sip/QtOpenGL/qglcolormap.sip ../sip/QtOpenGL/qglframebufferobject.sip ../sip/QtOpenGL/qglpixelbuffer.sip ../sip/QtOpenGL/qglshaderprogram.sip ../sip/QtOpenGL/QtOpenGLmod.sip
INSTALLS += sip
QMAKE_CXXFLAGS += -fno-exceptions
DEFINES += SIP_PROTECTED_IS_PUBLIC protected=public
INCLUDEPATH += .
INCLUDEPATH += /System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
INCLUDEPATH += ../qpy/QtOpenGL

win32 {
    QMAKE_POST_LINK = $(COPY_FILE) $(DESTDIR_TARGET) $$PY_MODULE
} else {
    QMAKE_POST_LINK = $(COPY_FILE) $(TARGET) $$PY_MODULE
}
macx {
    QMAKE_LFLAGS += "-undefined dynamic_lookup"
}

TARGET = QtOpenGL
HEADERS = sipAPIQtOpenGL.h ../qpy/QtOpenGL/qpyopengl_api.h
SOURCES = sipQtOpenGLcmodule.cpp sipQtOpenGLQGL.cpp sipQtOpenGLQGLBuffer.cpp sipQtOpenGLQGLColormap.cpp sipQtOpenGLQGLContext.cpp sipQtOpenGLQGLContextBindOptions.cpp sipQtOpenGLQGLFormat.cpp sipQtOpenGLQGLFormatOpenGLVersionFlags.cpp sipQtOpenGLQGLFormatOptions.cpp sipQtOpenGLQGLFramebufferObject.cpp sipQtOpenGLQGLFramebufferObjectFormat.cpp sipQtOpenGLQGLPixelBuffer.cpp sipQtOpenGLQGLShader.cpp sipQtOpenGLQGLShaderProgram.cpp sipQtOpenGLQGLShaderShaderType.cpp sipQtOpenGLQGLWidget.cpp sipQtOpenGLQList0101QGLShader.cpp ../qpy/QtOpenGL/qpyopengl_attribute_array.cpp ../qpy/QtOpenGL/qpyopengl_uniform_value_array.cpp

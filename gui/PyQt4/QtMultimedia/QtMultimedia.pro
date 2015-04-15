TEMPLATE = lib
CONFIG += warn_on plugin
QT += multimedia
CONFIG += release
CONFIG -= android_install

win32 {
    PY_MODULE = QtMultimedia.pyd
    target.files = QtMultimedia.pyd
    LIBS += -L/System/Library/Frameworks/Python.framework/Versions/2.7/lib -lpython27
} else {
    PY_MODULE = QtMultimedia.so
    target.files = QtMultimedia.so
}

target.CONFIG = no_check_exist
target.path = /System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/PyQt4
INSTALLS += target
sip.path = /System/Library/Frameworks/Python.framework/Versions/2.7/share/sip/PyQt4/QtMultimedia
sip.files = ../sip/QtMultimedia/qabstractvideobuffer.sip ../sip/QtMultimedia/qabstractvideosurface.sip ../sip/QtMultimedia/qaudio.sip ../sip/QtMultimedia/qaudiodeviceinfo.sip ../sip/QtMultimedia/qaudioformat.sip ../sip/QtMultimedia/qaudioinput.sip ../sip/QtMultimedia/qaudiooutput.sip ../sip/QtMultimedia/QtMultimediamod.sip ../sip/QtMultimedia/qvideoframe.sip ../sip/QtMultimedia/qvideosurfaceformat.sip
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

TARGET = QtMultimedia
HEADERS = sipAPIQtMultimedia.h
SOURCES = sipQtMultimediacmodule.cpp sipQtMultimediaQAbstractVideoBuffer.cpp sipQtMultimediaQAbstractVideoSurface.cpp sipQtMultimediaQAudio.cpp sipQtMultimediaQAudioDeviceInfo.cpp sipQtMultimediaQAudioFormat.cpp sipQtMultimediaQAudioInput.cpp sipQtMultimediaQAudioOutput.cpp sipQtMultimediaQList0100QAudioDeviceInfo.cpp sipQtMultimediaQList0100QAudioFormatEndian.cpp sipQtMultimediaQList0100QAudioFormatSampleType.cpp sipQtMultimediaQList0100QVideoFramePixelFormat.cpp sipQtMultimediaQVideoFrame.cpp sipQtMultimediaQVideoSurfaceFormat.cpp

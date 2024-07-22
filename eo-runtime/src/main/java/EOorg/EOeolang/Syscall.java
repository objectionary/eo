package EOorg.EOeolang;

import com.sun.jna.Native;
import com.sun.jna.Platform;

public class Syscall {
    public static native int getpid();

    static {
        Native.register(Platform.C_LIBRARY_NAME);
    }
}


package EOorg.EOeolang.EOsys;

import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * Interface to stdlib.
 * @since 0.1
 */
interface CStdLib extends Library {

    /**
     * C STDLIB instance.
     */
    CStdLib CSTDLIB = Native.load("c", CStdLib.class);

    int getpid();

    int write(final Long fd, final String buf, final Long size);

    int read(final Long fd, final byte[] buf, final Long size);
}

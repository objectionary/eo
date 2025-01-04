/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import EOorg.EOeolang.EOsys.Syscall;
import com.sun.jna.ptr.IntByReference;
import java.util.Arrays;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * ReadFile kernel32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile">here for details</a>
 * @since 0.40.0
 */
public final class ReadFileFuncCall implements Syscall {
    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public ReadFileFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final int size = new Dataized(params[1]).asNumber().intValue();
        final byte[] buf = new byte[(int) size];
        final IntByReference read = new IntByReference();
        final Phi result = this.win.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                Kernel32.INSTANCE.ReadFile(
                    Kernel32.INSTANCE.GetStdHandle(new Dataized(params[0]).asNumber().intValue()),
                    buf,
                    size,
                    read,
                    null
                )
            )
        );
        result.put(1, new Data.ToPhi(Arrays.copyOf(buf, read.getValue())));
        return result;
    }
}

/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package EOorg.EOeolang.EOsys.Posix; // NOPMD

import EOorg.EOeolang.EOsys.Syscall;
import com.sun.jna.Structure;
import java.util.Arrays;
import java.util.List;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Connect syscall.
 * @since 0.40
 */
public final class ConnectSyscall implements Syscall {
    /**
     * Posix object.
     */
    private final Phi posix;

    /**
     * Ctor.
     * @param posix Posix object
     */
    public ConnectSyscall(final Phi posix) {
        this.posix = posix;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.posix.take("return").copy();
        result.put(
            0,
            new Data.ToPhi(
                CStdLib.INSTANCE.connect(
                    new Dataized(params[0]).asNumber().intValue(),
                    new ConnectSyscall.SockaddrIn(
                        new Dataized(params[1].take("sin-family")).take(Short.class),
                        new Dataized(params[1].take("sin-port")).take(Short.class),
                        new Dataized(params[1].take("sin-addr")).take(Integer.class),
                        new Dataized(params[1].take("sin-zero")).take()
                    ),
                    new Dataized(params[2]).asNumber().intValue()
                )
            )
        );
        result.put(1, new PhDefault());
        return result;
    }

    /**
     * The sockaddr_in structure.
     * @since 0.40.0
     * @checkstyle VisibilityModifierCheck (50 lines)
     * @checkstyle ParameterNumberCheck (50 lines)
     */
    public static final class SockaddrIn extends Structure {
        /**
         * Address family (e.g., AF_INET).
         */
        public short family;

        /**
         * Port number in network byte order.
         */
        public short port;

        /**
         * IP address in network byte order.
         */
        public int addr;

        /**
         * Padding to match C structure.
         */
        public byte[] zero;

        /**
         * Convenient ctor for testing.
         * @param family Family
         * @param port Port
         * @param addr Address
         */
        public SockaddrIn(final short family, final short port, final int addr) {
            this(family, port, addr, new byte[] {0, 0, 0, 0, 0, 0, 0, 0});
        }

        /**
         * Ctor.
         * @param family Family
         * @param port Port
         * @param addr Address
         * @param zero Zero 8 bytes
         */
        SockaddrIn(final short family, final short port, final int addr, final byte[] zero) {
            this.family = family;
            this.port = port;
            this.addr = addr;
            this.zero = Arrays.copyOf(zero, zero.length);
        }

        @Override
        public List<String> getFieldOrder() {
            return Arrays.asList("family", "port", "addr", "zero");
        }
    }
}

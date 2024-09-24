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
package EOorg.EOeolang.EOsys; // NOPMD

import EOorg.EOeolang.EOsys.Posix.CStdLib;
import EOorg.EOeolang.EOtuple$EOempty;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

/**
 * Test case for {@link EOposix}.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@Execution(ExecutionMode.SAME_THREAD)
final class EOposixTest {
    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "The \"getpid\" system call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        Phi.Φ.take("org.eolang.sys.posix").copy(),
                        "name",
                        new Data.ToPhi("getpid")
                    ),
                    "args",
                    new EOtuple$EOempty()
                ).take("code")
            ).asNumber().intValue(),
            Matchers.equalTo(
                Integer.parseInt(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@")[0]
                )
            )
        );
    }

    @Test
    @Disabled
    void connectsSuccessfullyViaSyscalls() throws IOException {
        final ServerSocket socket = EOposixTest.startServer();
        final int descriptor = CStdLib.INSTANCE.socket(
            CStdLib.AF_INET,
            CStdLib.SOCK_STREAM,
            CStdLib.IPPROTO_TCP
        );
        assert descriptor >= 0;
        final int connected = CStdLib.INSTANCE.connect(
            descriptor,
            new SockaddrIn(
                (short) CStdLib.AF_INET,
                EOposixTest.htons(8080),
                CStdLib.INSTANCE.inet_addr("127.0.0.1")
            ),
            16
        );
        MatcherAssert.assertThat(
            "Libc 'connect' syscall should have successfully connected to local server, but it didn't",
            connected,
            Matchers.equalTo(0)
        );
        assert CStdLib.INSTANCE.close(descriptor) == 0;
        EOposixTest.stopServer(socket);
    }

    @Test
    void connectsViaSocketObject() throws IOException {
        final ServerSocket socket = EOposixTest.startServer();
        final Phi sock = Phi.Φ.take("org.eolang.net.socket")
            .take("posix-socket")
            .copy();
        sock.put(0, new Data.ToPhi("127.0.0.1"));
        sock.put(1, new Data.ToPhi(8080));
        final Phi connected = sock.take("connect").copy();
        connected.put(0, new EOposixTest.Scope());
        final byte[] result = new Dataized(connected).take();
        final byte[] expected = new byte[] {1};
        if (!Arrays.equals(result, expected)) {
            Logger.info(
                this,
                "Strerror: %s",
                CStdLib.INSTANCE.strerror(CStdLib.INSTANCE.errno())
            );
        }
        MatcherAssert.assertThat(
            String.format(
                "Posix socket should have connected successfully to local server, but it didn't, message is: '%s'",
                new String(result, StandardCharsets.UTF_8)
            ),
            result,
            Matchers.equalTo(expected)
        );
        EOposixTest.stopServer(socket);
    }


    /**
     * Start local server socket.
     * @return Local server socket
     * @throws IOException If fails
     */
    private static ServerSocket startServer() throws IOException {
        return new ServerSocket(8080);
    }

    /**
     * Stop local server.
     * @param socket Local server socket
     * @throws IOException If fails
     */
    private static void stopServer(final ServerSocket socket) throws IOException {
        if (socket != null && !socket.isClosed()) {
            socket.close();
        }
    }

    /**
     * Helper function to convert port number to network byte order (htons).
     * @param port Port
     * @return Port in network byte order
     */
    private static short htons(final int port) {
        return (short) (((port & 0xFF) << 8) | ((port >> 8) & 0xFF));
    }

    /**
     * Socket scope.
     * @since 0.40.0
     */
    private static final class Scope extends PhDefault implements Atom {
        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        Scope() {
            this.add("s", new AtVoid("s"));
        }

        @Override
        public Phi lambda() {
            return new Data.ToPhi(true);
        }
    }
}

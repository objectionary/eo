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
 */
package EOorg.EOeolang.EOsys;

import org.eolang.Data;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link DispatchedUnixSyscall}.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
final class DispatchedUnixSyscallTest {
    @Test
    @DisabledOnOs(OS.WINDOWS)
    void failsOnIllegalSyscall() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new DispatchedUnixSyscall("EnVeLoPe))").call(),
            "Expected \"IllegalArgumentException\" to be thrown on illegal syscall"
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesGetpidWithoutExceptions() {
        Assertions.assertDoesNotThrow(
            () -> new DispatchedUnixSyscall("getpid").call(),
            "Expected \"getpid\" syscall to be called without exceptions."
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "Expected \"getpid\" syscall to dispatched correctly",
            new DispatchedUnixSyscall("getpid").call(),
            Matchers.equalTo(CStdLib.CSTDLIB.getpid())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesWriteWithoutExceptions() {
        final String msg = "Hello, world!\n";
        Assertions.assertDoesNotThrow(
            () -> new DispatchedUnixSyscall("write").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi((long) msg.length())
            ),
            "Expected \"write\" syscall to be called without exceptions."
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesWriteCorrectly() {
        final String msg = "Hello, world!\n";
        MatcherAssert.assertThat(
            "Expected \"write\" syscall to dispatched correctly",
            new DispatchedUnixSyscall("write").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi((long) msg.length())
            ),
            Matchers.equalTo(msg.length())
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesReadWithoutExceptions() {
        final int size = 3;
        final byte[] buf = new byte[size];
        Assertions.assertDoesNotThrow(
            () -> new DispatchedUnixSyscall("read").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(buf),
                new Data.ToPhi(size)
            ),
            "Expected \"read\" syscall to be called without exceptions."
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void invokesReadFromStdoutWithError() {
        final int size = 3;
        final byte[] buf = new byte[size];
        MatcherAssert.assertThat(
            "Expected \"read\" syscall to dispatched correctly",
            new DispatchedUnixSyscall("read").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(buf),
                new Data.ToPhi(size)
            ),
            Matchers.equalTo(-1)
        );
    }
}

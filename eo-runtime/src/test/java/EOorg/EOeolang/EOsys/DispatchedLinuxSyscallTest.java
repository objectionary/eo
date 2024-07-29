package EOorg.EOeolang.EOsys;

import org.eolang.Data;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class DispatchedLinuxSyscallTest {
    @Test
    void failsOnIllegalSyscall() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new DispatchedLinuxSyscall("EnVeLoPe))").call()
        );
    }

    @Test
    void invokesGetpidWithoutExceptions() {
        Assertions.assertDoesNotThrow(
            () -> new DispatchedLinuxSyscall("getpid").call(),
            "Expected \"getpid\" syscall to be called without exceptions."
        );
    }

    @Test
    void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "Expected \"getpid\" syscall to dispatched correctly",
            new DispatchedLinuxSyscall("getpid").call(),
            Matchers.equalTo(CStdLib.CSTDLIB.getpid())
        );
    }

    @Test
    void invokesWriteWithoutExceptions() {
        final String msg = "Hello, world!\n";
        Assertions.assertDoesNotThrow(
            () -> new DispatchedLinuxSyscall("write").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi((long) msg.length())
            ),
            "Expected \"write\" syscall to be called without exceptions."
        );
    }

    @Test
    void invokesWriteCorrectly() {
        final String msg = "Hello, world!\n";
        MatcherAssert.assertThat(
            "Expected \"write\" syscall to dispatched correctly",
            new DispatchedLinuxSyscall("write").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi((long) msg.length())
            ),
            Matchers.equalTo(msg.length())
        );
    }

    @Test
    void invokesReadWithoutExceptions() {
        final int size = 3;
        final byte[] buf = new byte[size];
        Assertions.assertDoesNotThrow(
            () -> new DispatchedLinuxSyscall("read").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(buf),
                new Data.ToPhi(size)
            ),
            "Expected \"read\" syscall to be called without exceptions."
        );
    }

    @Test
    void invokesReadFromStdoutWithError() {
        final int size = 3;
        final byte[] buf = new byte[size];
        MatcherAssert.assertThat(
            "Expected \"read\" syscall to dispatched correctly",
            new DispatchedLinuxSyscall("read").call(
                new Data.ToPhi(1L),
                new Data.ToPhi(buf),
                new Data.ToPhi(size)
            ),
            Matchers.equalTo(-1)
        );
    }
}
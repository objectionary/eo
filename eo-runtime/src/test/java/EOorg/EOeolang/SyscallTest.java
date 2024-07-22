package EOorg.EOeolang;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class SyscallTest {
    @Test
    void executesCorrectly() {
        Assertions.assertDoesNotThrow(
            () -> System.out.println(Syscall.getpid())
        );
    }

}
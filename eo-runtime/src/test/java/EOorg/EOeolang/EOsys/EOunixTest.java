package EOorg.EOeolang.EOsys;

import EOorg.EOeolang.EOtuple$EOempty;
import java.lang.management.ManagementFactory;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOunix}.
 *
 * @since 0.39
 * @checkstyle TypeNameCheck (100 lines)
 */
public class EOunixTest {
    @Test
    @DisabledOnOs(OS.WINDOWS)
    public void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOunix(),
                        "name",
                        new Data.ToPhi("getpid")
                    ),
                    "args",
                    new EOtuple$EOempty()
                )
            ).take(Long.class),
            Matchers.equalTo(
                Long.parseLong(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@")[0]
                )
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    public void invokesWriteCorrectly() {
        final String msg = "Hello, world!\n";
        Phi args = new PhWith(
            new PhWith(
                new PhWith(
                    new EOtuple$EOempty().take("with").copy(),
                    0, new Data.ToPhi(1L)
                ).take("with").copy(),
                0, new Data.ToPhi(msg)
            ).take("with").copy(),
            0, new Data.ToPhi(msg.length())
        );
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOunix(),
                        "name",
                        new Data.ToPhi("write")
                    ),
                    "args",
                    args
                )
            ).take(Long.class),
            Matchers.equalTo(
                (long) msg.length()
            )
        );
    }
}

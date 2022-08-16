package EOorg.EOeolang;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import org.eolang.Data;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test for {@link Ram}.
 * @since 1.0
 */
@Execution(ExecutionMode.CONCURRENT)
final class RamTest {
    @ParameterizedTest
    @CsvSource({
        "5,  0, hello, 0, 5, hello",
        "10, 5, hello, 5, 5, hello",
        "13, 0, hello world, 6, 5, world"
    })
    void writesAndReads(
        final long total,
        final int wrt,
        final String data,
        final int rdr,
        final int len,
        final String result
    ) throws IOException {
        final Phi ref = new PhWith(new EOram(Phi.Φ), 0, new Data.ToPhi(total));
        Ram.INSTANCE.write(ref, wrt, data.getBytes(StandardCharsets.UTF_8));
        final byte[] bytes = Ram.INSTANCE.read(ref, rdr, len);
        MatcherAssert.assertThat(
            new String(bytes, StandardCharsets.UTF_8),
            Matchers.is(
                result
            )
        );
    }
}

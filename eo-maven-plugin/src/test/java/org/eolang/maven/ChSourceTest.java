package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link ChSource}.
 *
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class ChSourceTest {

    @Test
    void computesHashValueForTheFile(@Mktmp final Path dir) throws Exception {
        final Path file = dir.resolve("source.eo");
        Files.writeString(
            file,
            "[] > main\n" +
            "  (stdout \"Hello, EO!\") > @\n"
        );
        MatcherAssert.assertThat(
            "We should comput the correct hash (SHA-1) for the source file",
            new ChSource(file).value(),
            Matchers.equalTo("f97a8da03d184cab9a43e1288293391fb1ccc296")
        );
    }

}
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import org.cactoos.Text;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.hash.ChsAsMap;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

final public class VersionsMojoTest {
    @Test
    void replacesVersionsOk(@TempDir final Path tmp) throws Exception {
        final String[] tags = new String[] {"0.23.17", "0.25.0"};
        final String[] hashes = new String[] {"15c85d7", "0aa6875"};
        new FakeMaven(tmp)
            .with("withVersions", true)
            .with("commitHashes", new ChsAsMap.Fake())
            .withProgram(
                "+alias org.eolang.io.stdout\n",
                "[] > main",
                "  seq > @",
                "    stdout|0.23.17",
                "      QQ.txt.sprintf|0.25.0",
                "        \"Hello world\"",
                "    nop"
            ).execute(new FakeMaven.Versions());
        final XML xml = new XMLDocument(
            tmp.resolve(
                String.format("target/%s/foo/x/main.xmir", OptimizeMojo.DIR)
            )
        );
        final String format = "//o[@ver and (@ver='%s' or @ver='%s')]/@ver";
        MatcherAssert.assertThat(
            xml.xpath(String.format(format, hashes[0], hashes[1])),
            Matchers.hasSize(2)
        );
        MatcherAssert.assertThat(
            xml.xpath(String.format(format, tags[0], tags[1])),
            Matchers.empty()
        );
    }
}

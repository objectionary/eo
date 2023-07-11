package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import org.cactoos.Text;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.cactoos.text.*;
import org.eolang.maven.hash.CommitHashes;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Home;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.regex.Pattern;

public final class VersionsMojo extends SafeMojo {

    @Override
    void exec() throws IOException {
        final Collection<ForeignTojo> tojos = this.scopedTojos().notDiscovered();
        final CommitHashes hashes = new CommitHashes();
        for (final ForeignTojo tojo : tojos) {
            final Path src = tojo.optimized();
            final Text source = new Sticky(
                new IoCheckedText(
                    new TextOf(src)
                )
            );
            new SumOf(
                new Mapped<>(
                    tag -> {
                        new Home(this.targetDir.toPath()).save(
                            new Replaced(
                                source,
                                tag,
                                hashes.get(tag)
                            ),
                            src
                        );
                        return 1;
                    },
                    new ListOf<>(
                        new Filtered<>(
                            ver -> !ver.isEmpty() && Pattern.matches("[0-9]+\\.[0-9]+\\.[0-9]+", ver),
                            new XMLDocument(src).xpath("//o[@ver]/@ver")
                        )
                    )
                )
            ).intValue();
        }

    }
}

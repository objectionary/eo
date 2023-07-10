package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import org.eolang.maven.tojos.ForeignTojo;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;

public class VersionsMojo extends SafeMojo {
    @Override
    void exec() throws IOException {
        final Collection<ForeignTojo> tojos = this.scopedTojos().notDiscovered();
        for (final ForeignTojo tojo : tojos) {
            final Path src = tojo.optimized();
            final XML xml = new XMLDocument(src);
        }
    }
}

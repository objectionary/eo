package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.maven.AssembleMojo;
import org.eolang.maven.OptimizeMojo;
import org.eolang.maven.Place;

public class OptCached implements Optimization {

    private final Optimization delegate;
    private final Path cache;
    private final String hash;

    public OptCached(
        final Optimization delegate,
        final Path cache,
        final String hash
    ) {
        this.delegate = delegate;
        this.cache = cache;
        this.hash = hash;
    }

    @Override
    public XML optimize(final Path xml) throws OptimizationException {
        try {
            final String name = new XMLDocument(xml).xpath(
                "/program/@name").get(0);
            final Path path = new Place(name).make(
                cache.resolve(OptimizeMojo.OPTIMIZED)
                    .resolve(hash),
                AssembleMojo.ATTR_XMIR
            );
            final XML optimized;
            if (Files.exists(path)) {
                optimized = new XMLDocument(path);
            } else {
                optimized = delegate.optimize(xml);
                Files.createDirectories(path.getParent());
                Files.createFile(path);
                Files.write(
                    path,
                    optimized.toString()
                        .getBytes(StandardCharsets.UTF_8)
                );
            }
            return optimized;
        } catch (FileNotFoundException ex) {
            throw new OptimizationException("", ex); //todo
        } catch (IOException ex) {
            throw new OptimizationException("", ex); //todo
        }

    }
}

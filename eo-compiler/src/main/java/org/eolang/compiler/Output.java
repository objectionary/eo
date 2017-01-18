package org.eolang.compiler;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Output.
 *
 * @author Piotr Chmielowski (piotrek.chmielowski@interia.pl)
 * @version $Id$
 * @since 0.1
 */
public class Output {

    /**
     * Path to directory.
     */
    private final Path dir;

    /**
     * Ctor.
     *
     * @param path Path to directory
     */
    public Output(final Path path) {
        this.dir = path;
    }

    /**
     * Save content.
     *
     * @param file    File to save to
     * @param content Java content
     */
    void save(final Path file, final String content) {
        try {
            Files.write(dir.resolve(file), content.getBytes());
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }
}

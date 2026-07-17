/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Directory.tmpfile.touch.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "directory.tmpfile.touch")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOdirectory$EOtmpfile$EOtouch extends PhDefault implements Atom {

    @Override

    public Phi lambda() {
        final Path home = Paths.get(
            new Dataized(
                this.take(Phi.RHO).take(Phi.RHO).take("path")
            ).asString()
        );
        final Path path;
        try {
            path = Files.createTempFile(home, null, null);
        } catch (final IOException ex) {
            throw new ExFailure(
                String.format("Can't create a temp file in the directory '%s'", home),
                ex
            );
        }
        path.toFile().deleteOnExit();
        return new Data.ToPhi(path.toAbsolutePath().toString());
    }
}

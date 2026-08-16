/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * Packages that the eo-runtime build merges into the objects they belong to.
 *
 * <p>A sandbox that transpiles the runtime objects again must merge exactly
 * the same packages the runtime itself merges. Unmerged, a member reaching
 * its receiver through {@code ^} is applied through the first void it does
 * not have any more, and an atom of such a member is generated under a class
 * name the runtime jar does not carry. The list is read from the pom that
 * declares it, so a package named for merging there needs no second mention
 * here.</p>
 *
 * @since 0.63
 */
final class MergedPackages {

    /**
     * The pom that declares the packages to merge.
     */
    private final Path pom;

    /**
     * Ctor.
     */
    MergedPackages() {
        this(
            Paths.get(System.getProperty("basedir", System.getProperty("user.dir")))
                .getParent()
                .resolve("eo-runtime")
                .resolve("pom.xml")
        );
    }

    /**
     * Ctor.
     * @param file The pom that declares the packages to merge
     */
    MergedPackages(final Path file) {
        this.pom = file;
    }

    /**
     * Names of the packages to merge.
     * @return The names, as the pom lists them
     * @throws IOException If the pom cannot be read
     */
    List<String> names() throws IOException {
        return new XMLDocument(this.pom).xpath(
            "//*[local-name()='mergedPackage']/text()"
        );
    }
}

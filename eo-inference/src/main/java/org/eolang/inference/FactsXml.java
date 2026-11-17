/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;

/**
 * The file {@code facts.xml}, read into a {@link Program}.
 *
 * <p>The file of eo-runtime is twenty megabytes, so it is read as a stream,
 * an element at a time, by {@link FactsStream}.</p>
 *
 * @since 0.76.0
 */
final class FactsXml {

    /**
     * The file.
     */
    private final Path file;

    /**
     * Ctor.
     *
     * @param path The file
     */
    FactsXml(final Path path) {
        this.file = path;
    }

    /**
     * The program the file states.
     *
     * @return The program
     * @throws IOException If the file cannot be read
     */
    Program program() throws IOException {
        try (InputStream input = Files.newInputStream(this.file)) {
            final XMLInputFactory factory = XMLInputFactory.newInstance();
            factory.setProperty(XMLInputFactory.SUPPORT_DTD, false);
            return new FactsStream(factory.createXMLStreamReader(input, "UTF-8")).program();
        } catch (final XMLStreamException ex) {
            throw new IOException(String.format("Can't read the facts in %s", this.file), ex);
        }
    }
}

/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * The file {@code facts.xml}, read into a {@link Program}.
 *
 * <p>The file of eo-runtime is twenty megabytes, so it is read as a stream,
 * an element at a time, and nothing of it is kept but what the engine
 * reads: the voids of each formation and what each says of them, the
 * reads, the heads and the calls.</p>
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
            return FactsXml.program(factory.createXMLStreamReader(input, "UTF-8"));
        } catch (final XMLStreamException ex) {
            throw new IOException(String.format("Can't read the facts in %s", this.file), ex);
        }
    }

    /**
     * The program a stream of elements states.
     *
     * @param xml The stream
     * @return The program
     * @throws XMLStreamException If the stream breaks
     * @checkstyle CyclomaticComplexityCheck (100 lines)
     * @checkstyle ExecutableStatementCountCheck (100 lines)
     */
    @SuppressWarnings("PMD.CognitiveComplexity")
    private static Program program(final XMLStreamReader xml) throws XMLStreamException {
        final Map<String, Formation> forms = new LinkedHashMap<>(0);
        final Map<String, Read> reads = new LinkedHashMap<>(0);
        final Map<String, List<Head>> heads = new LinkedHashMap<>(0);
        final Map<String, List<String>> calls = new LinkedHashMap<>(0);
        String section = "";
        String id = "";
        Map<String, Attribute> attrs = new LinkedHashMap<>(0);
        List<String> order = new ArrayList<>(0);
        Map<String, String> notes = new HashMap<>(0);
        Map<String, List<String>> back = new HashMap<>(0);
        List<String> gives = new ArrayList<>(0);
        List<Head> starts = new ArrayList<>(0);
        List<String[]> args = new ArrayList<>(0);
        while (xml.hasNext()) {
            final int event = xml.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                final String tag = xml.getLocalName();
                if (Arrays.asList("objects", "reads", "heads", "calls").contains(tag)) {
                    section = tag;
                } else if ("objects".equals(section) && "type".equals(tag)) {
                    id = FactsXml.attr(xml, "id");
                    attrs = new LinkedHashMap<>(0);
                    order = new ArrayList<>(0);
                    notes = new HashMap<>(0);
                    back = new HashMap<>(0);
                    gives = new ArrayList<>(0);
                    if (!FactsXml.attr(xml, "returns").isEmpty()) {
                        notes.put("returns", FactsXml.attr(xml, "returns"));
                    }
                    notes.put("complete", FactsXml.attr(xml, "complete"));
                } else if ("objects".equals(section) && "attr".equals(tag)) {
                    final String name = FactsXml.attr(xml, "name");
                    final boolean open = "true".equals(FactsXml.attr(xml, "void"));
                    attrs.put(name, new Attribute(open, FactsXml.attr(xml, "holds")));
                    if (open && !FactsXml.attr(xml, "args").isEmpty()) {
                        back.put(
                            name,
                            Arrays.asList(
                                FactsXml.attr(xml, "args").replace(',', ' ').trim().split("\\s+")
                            )
                        );
                    }
                    if ("ρ".equals(name)) {
                        notes.remove("rho");
                        if (!FactsXml.attr(xml, "rho").isEmpty()) {
                            notes.put("rho", FactsXml.attr(xml, "rho"));
                        }
                    } else if (open) {
                        order.add(name);
                    }
                } else if ("objects".equals(section) && "return".equals(tag)) {
                    gives.add(FactsXml.attr(xml, "loc"));
                } else if ("reads".equals(section) && "read".equals(tag)) {
                    reads.put(
                        FactsXml.attr(xml, "id"),
                        new Read(FactsXml.attr(xml, "name"), FactsXml.attr(xml, "of"))
                    );
                } else if ("heads".equals(section) && "type".equals(tag)) {
                    id = FactsXml.attr(xml, "id");
                    starts = new ArrayList<>(1);
                } else if ("heads".equals(section)) {
                    starts.add(new Head(tag, FactsXml.attr(xml, "loc")));
                } else if ("calls".equals(section) && "call".equals(tag)) {
                    id = FactsXml.attr(xml, "loc");
                    args = new ArrayList<>(1);
                } else if ("calls".equals(section) && "arg".equals(tag)) {
                    args.add(new String[] {FactsXml.attr(xml, "n"), FactsXml.attr(xml, "loc")});
                }
            } else if (event == XMLStreamConstants.END_ELEMENT) {
                final String tag = xml.getLocalName();
                if ("objects".equals(section) && "type".equals(tag)) {
                    forms.put(id, new Formation(attrs, order, notes, back, gives));
                } else if ("heads".equals(section) && "type".equals(tag)) {
                    heads.put(id, starts);
                } else if ("calls".equals(section) && "call".equals(tag)) {
                    args.sort(Comparator.comparingInt(arg -> Integer.parseInt(arg[0])));
                    final List<String> locs = new ArrayList<>(args.size());
                    for (final String[] arg : args) {
                        locs.add(arg[1]);
                    }
                    calls.put(id, locs);
                }
            }
        }
        return new Program(forms, reads, heads, calls);
    }

    /**
     * An attribute of the element the stream stands at.
     *
     * @param xml The stream
     * @param name The name of the attribute
     * @return Its value, empty when it is absent
     */
    private static String attr(final XMLStreamReader xml, final String name) {
        final String value = xml.getAttributeValue(null, name);
        final String found;
        if (value == null) {
            found = "";
        } else {
            found = value;
        }
        return found;
    }
}

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
import java.util.regex.Pattern;
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
     * The spaces between the types of the arguments an atom calls a void
     * back with.
     */
    private static final Pattern SPACES = Pattern.compile("\\s+");

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

    private static Program program(final XMLStreamReader xml) throws XMLStreamException {
        final Map<String, Formation> forms = new LinkedHashMap<>(0);
        final Map<String, Read> reads = new LinkedHashMap<>(0);
        final Map<String, List<Head>> heads = new LinkedHashMap<>(0);
        final Map<String, List<String>> calls = new LinkedHashMap<>(0);
        while (xml.hasNext()) {
            if (xml.next() == XMLStreamConstants.START_ELEMENT) {
                final String tag = xml.getLocalName();
                if ("objects".equals(tag)) {
                    FactsXml.objects(xml, forms);
                } else if ("reads".equals(tag)) {
                    FactsXml.reads(xml, reads);
                } else if ("heads".equals(tag)) {
                    FactsXml.heads(xml, heads);
                } else if ("calls".equals(tag)) {
                    FactsXml.calls(xml, calls);
                }
            }
        }
        return new Program(forms, reads, heads, calls);
    }

    private static void objects(
        final XMLStreamReader xml, final Map<String, Formation> forms
    ) throws XMLStreamException {
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "objects")) {
            if (FactsXml.opens(xml, event, "type")) {
                forms.put(FactsXml.attr(xml, "id"), FactsXml.formation(xml));
            }
            event = xml.next();
        }
    }

    private static Formation formation(final XMLStreamReader xml) throws XMLStreamException {
        final Map<String, Attribute> attrs = new LinkedHashMap<>(0);
        final List<String> order = new ArrayList<>(0);
        final Map<String, String> notes = new HashMap<>(0);
        final Map<String, List<String>> back = new HashMap<>(0);
        final List<String> gives = new ArrayList<>(0);
        if (!FactsXml.attr(xml, "returns").isEmpty()) {
            notes.put("returns", FactsXml.attr(xml, "returns"));
        }
        notes.put("complete", FactsXml.attr(xml, "complete"));
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "type")) {
            if (FactsXml.opens(xml, event, "attr")) {
                FactsXml.attribute(xml, attrs, order, notes, back);
            } else if (FactsXml.opens(xml, event, "return")) {
                gives.add(FactsXml.attr(xml, "loc"));
            }
            event = xml.next();
        }
        return new Formation(attrs, order, notes, back, gives);
    }

    private static void attribute(
        final XMLStreamReader xml, final Map<String, Attribute> attrs, final List<String> order,
        final Map<String, String> notes, final Map<String, List<String>> back
    ) {
        final String name = FactsXml.attr(xml, "name");
        final boolean open = "true".equals(FactsXml.attr(xml, "void"));
        attrs.put(name, new Attribute(open, FactsXml.attr(xml, "holds")));
        if (open && !FactsXml.attr(xml, "args").isEmpty()) {
            back.put(
                name,
                Arrays.asList(
                    FactsXml.SPACES.split(
                        FactsXml.attr(xml, "args").replace(',', ' ').trim()
                    )
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
    }

    private static void reads(
        final XMLStreamReader xml, final Map<String, Read> reads
    ) throws XMLStreamException {
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "reads")) {
            if (FactsXml.opens(xml, event, "read")) {
                reads.put(
                    FactsXml.attr(xml, "id"),
                    new Read(FactsXml.attr(xml, "name"), FactsXml.attr(xml, "of"))
                );
            }
            event = xml.next();
        }
    }

    private static void heads(
        final XMLStreamReader xml, final Map<String, List<Head>> heads
    ) throws XMLStreamException {
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "heads")) {
            if (FactsXml.opens(xml, event, "type")) {
                heads.put(FactsXml.attr(xml, "id"), FactsXml.starts(xml));
            }
            event = xml.next();
        }
    }

    private static List<Head> starts(final XMLStreamReader xml) throws XMLStreamException {
        final List<Head> starts = new ArrayList<>(1);
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "type")) {
            if (event == XMLStreamConstants.START_ELEMENT) {
                starts.add(new Head(xml.getLocalName(), FactsXml.attr(xml, "loc")));
            }
            event = xml.next();
        }
        return starts;
    }

    private static void calls(
        final XMLStreamReader xml, final Map<String, List<String>> calls
    ) throws XMLStreamException {
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "calls")) {
            if (FactsXml.opens(xml, event, "call")) {
                calls.put(FactsXml.attr(xml, "loc"), FactsXml.args(xml));
            }
            event = xml.next();
        }
    }

    private static List<String> args(final XMLStreamReader xml) throws XMLStreamException {
        final List<String[]> args = new ArrayList<>(1);
        int event = xml.next();
        while (!FactsXml.closes(xml, event, "call")) {
            if (FactsXml.opens(xml, event, "arg")) {
                args.add(new String[] {FactsXml.attr(xml, "n"), FactsXml.attr(xml, "loc")});
            }
            event = xml.next();
        }
        args.sort(Comparator.comparingInt(arg -> Integer.parseInt(arg[0])));
        final List<String> locs = new ArrayList<>(args.size());
        for (final String[] arg : args) {
            locs.add(arg[1]);
        }
        return locs;
    }

    private static boolean opens(final XMLStreamReader xml, final int event, final String tag) {
        return event == XMLStreamConstants.START_ELEMENT && tag.equals(xml.getLocalName());
    }

    private static boolean closes(final XMLStreamReader xml, final int event, final String tag) {
        return event == XMLStreamConstants.END_ELEMENT && tag.equals(xml.getLocalName());
    }

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

/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * The file {@code facts.xml} as it is read, an element at a time.
 *
 * <p>Each section of the file is read where the stream stands, up to its
 * closing tag, and nothing of it is kept but what the engine reads: the
 * voids of each formation and what each says of them, the reads, the heads
 * and the calls.</p>
 *
 * @since 0.76.0
 */
final class FactsStream {

    /**
     * The spaces between the types of the arguments an atom calls a void
     * back with.
     */
    private static final Pattern SPACES = Pattern.compile("\\s+");

    /**
     * The stream of the file.
     */
    private final XMLStreamReader xml;

    /**
     * Ctor.
     *
     * @param stream The stream of the file
     */
    FactsStream(final XMLStreamReader stream) {
        this.xml = stream;
    }

    /**
     * The program the file states.
     *
     * @return The program
     * @throws XMLStreamException If the stream cannot be read
     */
    Program program() throws XMLStreamException {
        final Map<String, Formation> forms = new LinkedHashMap<>(0);
        final Map<String, Read> reads = new LinkedHashMap<>(0);
        final Map<String, List<Head>> heads = new LinkedHashMap<>(0);
        final Map<String, List<Argument>> calls = new LinkedHashMap<>(0);
        while (this.xml.hasNext()) {
            if (this.xml.next() == XMLStreamConstants.START_ELEMENT) {
                final String tag = this.xml.getLocalName();
                if ("objects".equals(tag)) {
                    this.objects(forms);
                } else if ("reads".equals(tag)) {
                    this.reads(reads);
                } else if ("heads".equals(tag)) {
                    this.heads(heads);
                } else if ("calls".equals(tag)) {
                    this.calls(calls);
                }
            }
        }
        return new Program(forms, reads, heads, calls);
    }

    private void objects(final Map<String, Formation> forms) throws XMLStreamException {
        int event = this.xml.next();
        while (!this.closes(event, "objects")) {
            if (this.opens(event, "type")) {
                forms.put(this.attr("id"), this.formation());
            }
            event = this.xml.next();
        }
    }

    private Formation formation() throws XMLStreamException {
        final Map<String, Attribute> attrs = new LinkedHashMap<>(0);
        final List<String> order = new ArrayList<>(0);
        final Map<String, String> notes = new HashMap<>(0);
        final Map<String, List<String>> back = new HashMap<>(0);
        final List<String> gives = new ArrayList<>(0);
        final String returns = this.attr("returns");
        if (!returns.isEmpty()) {
            notes.put("returns", returns);
        }
        notes.put("complete", this.attr("complete"));
        int event = this.xml.next();
        while (!this.closes(event, "type")) {
            if (this.opens(event, "attr")) {
                this.attribute(attrs, order, notes, back);
            } else if (this.opens(event, "return")) {
                gives.add(this.attr("loc"));
            }
            event = this.xml.next();
        }
        return new Formation(attrs, order, notes, back, gives);
    }

    private void attribute(
        final Map<String, Attribute> attrs, final List<String> order,
        final Map<String, String> notes, final Map<String, List<String>> back
    ) {
        final String name = this.attr("name");
        final boolean open = "true".equals(this.attr("void"));
        attrs.put(name, new Attribute(open, this.attr("holds")));
        if (open && !this.attr("args").isEmpty()) {
            back.put(
                name,
                Arrays.asList(
                    FactsStream.SPACES.split(this.attr("args").replace(',', ' ').trim())
                )
            );
        }
        if ("ρ".equals(name)) {
            notes.remove("rho");
            if (!this.attr("rho").isEmpty()) {
                notes.put("rho", this.attr("rho"));
            }
        } else if (open) {
            order.add(name);
        }
    }

    private void reads(final Map<String, Read> reads) throws XMLStreamException {
        int event = this.xml.next();
        while (!this.closes(event, "reads")) {
            if (this.opens(event, "read")) {
                reads.put(this.attr("id"), new Read(this.attr("name"), this.attr("of")));
            }
            event = this.xml.next();
        }
    }

    private void heads(final Map<String, List<Head>> heads) throws XMLStreamException {
        int event = this.xml.next();
        while (!this.closes(event, "heads")) {
            if (this.opens(event, "type")) {
                heads.put(this.attr("id"), this.starts());
            }
            event = this.xml.next();
        }
    }

    private List<Head> starts() throws XMLStreamException {
        final List<Head> starts = new ArrayList<>(1);
        int event = this.xml.next();
        while (!this.closes(event, "type")) {
            if (event == XMLStreamConstants.START_ELEMENT) {
                starts.add(new Head(this.xml.getLocalName(), this.attr("loc")));
            }
            event = this.xml.next();
        }
        return starts;
    }

    private void calls(final Map<String, List<Argument>> calls) throws XMLStreamException {
        int event = this.xml.next();
        while (!this.closes(event, "calls")) {
            if (this.opens(event, "call")) {
                final String loc = this.attr("loc");
                calls.put(loc, this.args(loc));
            }
            event = this.xml.next();
        }
    }

    private List<Argument> args(final String call) throws XMLStreamException {
        final List<Argument> args = new ArrayList<>(1);
        int event = this.xml.next();
        while (!this.closes(event, "call")) {
            if (this.opens(event, "arg")) {
                args.add(this.argument());
            }
            event = this.xml.next();
        }
        if (args.stream().map(arg -> arg.name().isEmpty()).distinct().count() > 1) {
            throw new IllegalStateException(
                String.format("The call at %s binds its arguments both by slot and by name", call)
            );
        }
        args.sort(Comparator.comparingInt(Argument::slot));
        return args;
    }

    private Argument argument() {
        final Argument arg;
        if (this.attr("n").isEmpty()) {
            arg = new Argument(this.attr("loc"), -1, this.attr("name"));
        } else {
            arg = new Argument(this.attr("loc"), Integer.parseInt(this.attr("n")), "");
        }
        return arg;
    }

    private boolean opens(final int event, final String tag) {
        return event == XMLStreamConstants.START_ELEMENT && tag.equals(this.xml.getLocalName());
    }

    private boolean closes(final int event, final String tag) {
        return event == XMLStreamConstants.END_ELEMENT && tag.equals(this.xml.getLocalName());
    }

    private String attr(final String name) {
        String found = "";
        for (int idx = 0; idx < this.xml.getAttributeCount(); ++idx) {
            if (name.equals(this.xml.getAttributeLocalName(idx))) {
                found = this.xml.getAttributeValue(idx);
                break;
            }
        }
        return found;
    }
}

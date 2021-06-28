/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven.transpiler.xml2medium;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathFactory;
import org.eolang.maven.transpiler.mediumcodemodel.EOAbstraction;
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceEntity;
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceFile;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Performs parsing of XML documents of the 3rd stage to objects of the medium model.
 * @since 1.0
 */
public final class Xml2MediumParser {

    /**
     * File containing an XML document of the 3rd stage to be parsed.
     */
    private final File file;

    /**
     * XPath object used to query XML documents.
     */
    private XPath xpath;

    /**
     * Parsed XML document.
     */
    private Document doc;

    /**
     * Instantiates an XML-to-medium parser for the corresponding file.
     * @param file The XML file to be parsed.
     */
    public Xml2MediumParser(final File file) {
        this.file = file;
    }

    /**
     * Parses the XML document representing the EO source file into the corresponding Java object.
     * @return A Java object which represents the parsed source file contents.
     * @throws Xml2MediumParserException Raised when the parsing process fails.
     */
    public EOSourceEntity parse() throws Xml2MediumParserException {
        this.loadXmlDocument();
        final EOSourceFile sourcefile =
            FileMetadataParsingUtils.parseSourceFile(this.file, this.doc, this.xpath);
        final ArrayList<EOAbstraction> objects =
            ObjectsParsingUtils.parseObjects(this.file, this.doc, this.xpath, sourcefile);
        final EOAbstraction[] abstractions = new EOAbstraction[objects.size()];
        sourcefile.addObjects(objects.toArray(abstractions));
        return sourcefile;
    }

    /**
     * Loads the XML document and instantiates querying routines.
     * @throws Xml2MediumParserException Raised when the process of loading fails.
     */
    private void loadXmlDocument() throws Xml2MediumParserException {
        final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = null;
        try {
            builder = factory.newDocumentBuilder();
        } catch (final ParserConfigurationException exception) {
            throw new Xml2MediumParserException(
                String.format(
                    "Failed to create a document builder for the %s file.",
                    this.file.getName()
                ),
                exception
            );
        }
        try {
            this.doc = builder.parse(this.file);
        } catch (final SAXException | IOException exception) {
            throw new Xml2MediumParserException(
                String.format(
                    "Failed to parse an xml document of the %s file.",
                    this.file.getName()
                ),
                exception
            );
        }
        this.xpath = XPathFactory.newInstance().newXPath();
    }

    /**
     * An exception indicating that the parsing process has failed.
     * @since 1.0
     */
    public static class Xml2MediumParserException extends Exception {
        /**
         * Serialization marker.
         */
        private static final long serialVersionUID = -8443480134301042248L;

        /**
         * Instantiates an exception indicating that the parsing process has failed.
         * @param message The message of the error being indicated.
         */
        public Xml2MediumParserException(final String message) {
            super(message);
        }

        /**
         * Instantiates an exception indicating that the parsing process has failed.
         * @param message The message of the error being indicated.
         * @param cause Another exception that caused this one.
         */
        public Xml2MediumParserException(final String message, final Exception cause) {
            super(message, cause);
        }
    }
}

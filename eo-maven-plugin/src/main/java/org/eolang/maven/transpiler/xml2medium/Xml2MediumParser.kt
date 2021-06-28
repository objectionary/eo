/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2021 nlchar
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
package org.eolang.maven.transpiler.xml2medium

import org.eolang.maven.transpiler.mediumcodemodel.EOAbstraction
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceEntity
import org.eolang.maven.transpiler.xml2medium.FileMetadataParsingUtils.parseSourceFile
import org.eolang.maven.transpiler.xml2medium.ObjectsParsingUtils.parseObjects
import org.w3c.dom.Document
import org.xml.sax.SAXException
import java.io.File
import java.io.IOException
import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.ParserConfigurationException
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathFactory

/**
 * Performs parsing of XML documents of the 3rd stage to objects of the medium model.
 * @since 1.0
 */
class Xml2MediumParser
/**
 * Instantiates an XML-to-medium parser for the corresponding file.
 * @param file The XML file to be parsed.
 */(
    /**
     * File containing an XML document of the 3rd stage to be parsed.
     */
    private val file: File
) {
    /**
     * XPath object used to query XML documents.
     */
    private lateinit var xpath: XPath

    /**
     * Parsed XML document.
     */
    private lateinit var doc: Document

    /**
     * Parses the XML document representing the EO source file into the corresponding Java object.
     * @return A Java object which represents the parsed source file contents.
     * @throws Xml2MediumParserException Raised when the parsing process fails.
     */
    @Throws(Xml2MediumParserException::class)
    fun parse(): EOSourceEntity {
        loadXmlDocument()
        val sourcefile = parseSourceFile(file, doc, xpath)
        val objects = parseObjects(file, doc, xpath, sourcefile)
        val abstractions = arrayOfNulls<EOAbstraction>(objects.size)
        sourcefile.addObjects(*objects.toArray(abstractions) as Array<out EOAbstraction>)
        return sourcefile
    }

    /**
     * Loads the XML document and instantiates querying routines.
     * @throws Xml2MediumParserException Raised when the process of loading fails.
     */
    @Throws(Xml2MediumParserException::class)
    private fun loadXmlDocument() {
        val factory = DocumentBuilderFactory.newInstance()
        val builder: DocumentBuilder = try {
            factory.newDocumentBuilder()
        } catch (exception: ParserConfigurationException) {
            throw Xml2MediumParserException(
                String.format(
                    "Failed to create a document builder for the %s file.",
                    file.name
                ),
                exception
            )
        }
        try {
            doc = builder.parse(file)
        } catch (exception: SAXException) {
            throw Xml2MediumParserException(
                String.format(
                    "Failed to parse an xml document of the %s file.",
                    file.name
                ),
                exception
            )
        } catch (exception: IOException) {
            throw Xml2MediumParserException(
                String.format(
                    "Failed to parse an xml document of the %s file.",
                    file.name
                ),
                exception
            )
        }
        xpath = XPathFactory.newInstance().newXPath()
    }

    /**
     * An exception indicating that the parsing process has failed.
     * @since 1.0
     */
    class Xml2MediumParserException : Exception {
        /**
         * Instantiates an exception indicating that the parsing process has failed.
         * @param message The message of the error being indicated.
         */
        constructor(message: String?) : super(message)

        /**
         * Instantiates an exception indicating that the parsing process has failed.
         * @param message The message of the error being indicated.
         * @param cause Another exception that caused this one.
         */
        constructor(message: String?, cause: Exception?) : super(message, cause)

        companion object {
            /**
             * Serialization marker.
             */
            private const val serialVersionUID = -8443480134301042248L
        }
    }
}
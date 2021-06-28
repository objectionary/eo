package org.eolang.maven.transpiler.xml2medium

import org.eolang.maven.transpiler.mediumcodemodel.EOPackage
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceFile
import org.eolang.maven.transpiler.xml2medium.Xml2MediumParser.Xml2MediumParserException
import org.w3c.dom.DOMException
import org.w3c.dom.Document
import org.w3c.dom.NodeList
import java.io.File
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathConstants

object FileMetadataParsingUtils {
    private const val PROGRAM_TAG = "program"
    private const val NAME_ATTR = "name"

    @JvmStatic
    @Throws(Xml2MediumParserException::class)
    fun parseSourceFile(file: File, doc: Document, xpath: XPath): EOSourceFile {
        val sourceFileName = parseSourceFileName(file, doc)
        val sourceFilePackageName = parseSourceFilePackageName(file, doc, xpath)
        val eoPackage = EOPackage(sourceFilePackageName)
        val sourceFile = EOSourceFile(sourceFileName, eoPackage)
        eoPackage.addFile(sourceFile)
        return sourceFile
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseSourceFileName(file: File, doc: Document): String {
        // locating and retrieving the <program> tag
        val programTags = doc.getElementsByTagName(PROGRAM_TAG)
        if (programTags.length == 0) {
            throw Xml2MediumParserException("File " + file.name + " contains no <program> tag. There must be exactly one <program> tag per each file.")
        }
        if (programTags.length > 1) {
            throw Xml2MediumParserException("File " + file.name + " contains several <program> tags. There must be exactly one <program> tag per each file.")
        }
        val programTag = programTags.item(0)

        // extracting the 'name' attribute which contains the source file name
        val programTagNameAttr = programTag.attributes.getNamedItem(
            NAME_ATTR
        )
            ?: throw Xml2MediumParserException("File " + file.name + " contains the <program> tag without the 'name' attribute. The 'name' attribute is required.")
        val sourceFileName: String?
        sourceFileName = try {
            programTagNameAttr.nodeValue
        } catch (e: DOMException) {
            throw Xml2MediumParserException("File " + file.name + ": the attribute 'name' of the <program> tag is too long to be parsed.")
        }
        if (sourceFileName == null || sourceFileName.isEmpty()) {
            throw Xml2MediumParserException("File " + file.name + ": the attribute 'name' of the <program> tag is empty. The 'name' attribute is required and must contain the name of the source file.")
        }
        return sourceFileName
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseSourceFilePackageName(file: File, doc: Document, xpath: XPath): String {
        val packages: NodeList
        packages = try {
            xpath.evaluate(
                "/program/metas/meta[.//head[text()='package']]/tail/text()",
                doc,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("Internal error occurred while parsing the package name in File " + file.name + ".")
        }
        if (packages.length == 0) {
            throw Xml2MediumParserException("File " + file.name + " contains no package declaration. There must be exactly one package declaration as a <meta> tag in <program>/<metas>.")
        }
        if (packages.length > 1) {
            throw Xml2MediumParserException("File " + file.name + " contains several package declarations. There must be exactly one package declaration as a <meta> tag in <program>/<metas>.")
        }
        val packageTag = packages.item(0)
        val packageName: String? = try {
            packageTag.nodeValue
        } catch (e: DOMException) {
            throw Xml2MediumParserException("File " + file.name + ": the package name in <program>/<metas> is too long to be parsed.")
        }
        if (packageName == null || packageName.isEmpty()) {
            throw Xml2MediumParserException("File " + file.name + ": the package name in <program>/<metas> is empty. The package name must be present.")
        }
        return packageName
    }
}
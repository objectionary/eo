package org.eolang.maven.transpiler.xml2medium;

import org.eolang.maven.transpiler.mediumcodemodel.EOPackage;
import org.eolang.maven.transpiler.mediumcodemodel.EOSourceFile;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import java.io.File;

public class FileMetadataParsingUtils {
    private static final String PROGRAM_TAG = "program";
    private static final String NAME_ATTR = "name";

    public static EOSourceFile parseSourceFile(File file, Document doc, XPath xpath) throws XML2MediumParser.XML2MediumParserException {
        String sourceFileName = parseSourceFileName(file, doc);
        String sourceFilePackageName = parseSourceFilePackageName(file, doc, xpath);

        EOPackage eoPackage = new EOPackage(sourceFilePackageName);
        EOSourceFile sourceFile = new EOSourceFile(sourceFileName, eoPackage);
        eoPackage.addFile(sourceFile);

        return sourceFile;
    }

    private static String parseSourceFileName(File file, Document doc) throws XML2MediumParser.XML2MediumParserException {
        /* locating and retrieving the <program> tag */
        NodeList programTags = doc.getElementsByTagName(PROGRAM_TAG);
        if (programTags.getLength() == 0) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + " contains no <program> tag. There must be exactly one <program> tag per each file.");
        }
        if (programTags.getLength() > 1) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + " contains several <program> tags. There must be exactly one <program> tag per each file.");
        }
        Node programTag = programTags.item(0);

        /* extracting the 'name' attribute which contains the source file name */
        Node programTagNameAttr = programTag.getAttributes().getNamedItem(NAME_ATTR);
        if (programTagNameAttr == null) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + " contains the <program> tag without the 'name' attribute. The 'name' attribute is required.");
        }
        String sourceFileName;
        try {
            sourceFileName = programTagNameAttr.getNodeValue();
        } catch (DOMException e) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + ": the attribute 'name' of the <program> tag is too long to be parsed.");
        }
        if (sourceFileName == null || sourceFileName.isEmpty()) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + ": the attribute 'name' of the <program> tag is empty. The 'name' attribute is required and must contain the name of the source file.");
        }

        return sourceFileName;
    }

    private static String parseSourceFilePackageName(File file, Document doc, XPath xpath) throws XML2MediumParser.XML2MediumParserException {
        NodeList packages;
        try {
            packages = (NodeList) xpath.evaluate
                    (
                            "/program/metas/meta[.//head[text()='package']]/tail/text()",
                            doc,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing the package name in File " + file.getName() + ".");
        }

        if (packages.getLength() == 0) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + " contains no package declaration. There must be exactly one package declaration as a <meta> tag in <program>/<metas>.");
        }
        if (packages.getLength() > 1) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + " contains several package declarations. There must be exactly one package declaration as a <meta> tag in <program>/<metas>.");
        }
        Node packageTag = packages.item(0);

        String packageName;
        try {
            packageName = packageTag.getNodeValue();
        } catch (DOMException e) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + ": the package name in <program>/<metas> is too long to be parsed.");
        }
        if (packageName == null || packageName.isEmpty()) {
            throw new XML2MediumParser.XML2MediumParserException("File " + file.getName() + ": the package name in <program>/<metas> is empty. The package name must be present.");
        }

        return packageName;
    }
}

package org.eolang.maven.transpiler.xml2medium

import org.eolang.hse.core.EOObject
import org.eolang.maven.transpiler.mediumcodemodel.*
import org.eolang.maven.transpiler.xml2medium.Xml2MediumParser.Xml2MediumParserException
import org.w3c.dom.*
import java.io.File
import java.util.*
import java.util.regex.Pattern
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathConstants

object ObjectsParsingUtils {
    private const val NAME_ATTR = "name"
    private const val ORIGINAL_NAME_ATTR = "original-name"
    private const val LINE_ATTR = "line"
    private const val VARARG_ATTR = "vararg"
    private const val METHOD_ATTR = "method"
    private const val BASE_ATTR = "base"
    private const val DATA_ATTR = "data"

    @JvmStatic
    @Throws(Xml2MediumParserException::class)
    fun parseObjects(
        file: File,
        doc: Document,
        xPath: XPath,
        fileScope: EOSourceFile
    ): ArrayList<EOAbstraction> {
        var abstractions = parseAbstractions(file, doc, xPath)
        abstractions.addAll(parseGlobalApplications(file, doc, xPath))
        abstractions = deflateAbstractions(abstractions, fileScope)
        return abstractions
    }

    @Throws(Xml2MediumParserException::class)
    private fun deflateAbstractions(
        flattenedAbstractions: ArrayList<EOAbstraction>,
        fileScope: EOSourceFile
    ): ArrayList<EOAbstraction> {
        val fileLevelAbstractions = ArrayList<EOAbstraction>()
        for (i in flattenedAbstractions.indices) {
            val abstraction = flattenedAbstractions[i]
            val xmlName = abstraction.xmlName
            // analyzing components of the xml name (these reflect the hierarchical structure of the abstraction)
            var index: Int
            index = if (!abstraction.instanceName.isPresent) {
                // anonymous abstraction
                lastIndexOfRegex(xmlName, "((\\$\\d+)+\\$\\α\\d+)")
            } else {
                // normal named abstraction
                xmlName.lastIndexOf('$')
            }
            if (index == -1) {
                // this abstraction is not nested (it is in the file-level scope)
                abstraction.scope = fileScope
                fileLevelAbstractions.add(abstraction)
            } else {
                // this abstraction is nested (and we have to find its scope)
                val parentXmlName = xmlName.substring(0, index)
                deflateAbstraction(
                    flattenedAbstractions,
                    abstraction,
                    parentXmlName,
                    fileScope.fileName
                )
            }
        }
        return fileLevelAbstractions
    }

    @Throws(Xml2MediumParserException::class)
    private fun deflateAbstraction(
        flattenedAbstractions: ArrayList<EOAbstraction>,
        nestedAbstraction: EOAbstraction,
        parentXmlName: String,
        fileName: String
    ) {
        val name = nestedAbstraction.instanceName
        val parent = flattenedAbstractions
            .stream()
            .filter { abstraction: EOAbstraction -> abstraction.xmlName == parentXmlName }
            .findFirst()
        if (!parent.isPresent) {
            throw Xml2MediumParserException("File " + fileName + " > abstraction " + nestedAbstraction.xmlName + ": could not find its parent with the name " + parentXmlName)
        }

        // this abstraction is scoped to its parent
        nestedAbstraction.scope = parent.get()
        if (!name.isPresent || name.get() == "@") {
            // we do not need to reference
            // anonymous abstractions and abstraction-based decorations
            // as sub-abstractions
            // we only need to dereference the applications referencing them
            dereferenceWrapperApplications(parent.get(), nestedAbstraction, fileName)
        } else {
            // this is an abstraction-based object attribute
            // so, we add it as a sub-abstraction
            parent.get().addSubAbstraction(nestedAbstraction)
            // and dereference the application referencing it
            dereferenceWrapperApplications(parent.get(), nestedAbstraction, fileName)
        }
    }

    @Throws(Xml2MediumParserException::class)
    private fun dereferenceWrapperApplications(
        parent: EOAbstraction,
        wrappedAbstraction: EOAbstraction,
        fileName: String
    ) {
        val boundAttributes = parent.boundAttributes
        if (wrappedAbstraction.instanceName.isPresent) {
            // for named objects we find their sole wrapper
            val wrappers: Array<EOApplication>? = boundAttributes
                ?.stream()
                ?.filter { eoApplication: EOApplication -> eoApplication.appliedObject == wrappedAbstraction.xmlName }
                ?.toArray { arrayOfNulls<EOApplication>(it) }
            if (wrappers == null || wrappers.isEmpty()) {
                throw Xml2MediumParserException("File " + fileName + " > abstraction " + parent.xmlName + ": there is no application-based wrapper for the sub-abstraction " + wrappedAbstraction.xmlName)
            }
            if (wrappers.size > 1) {
                throw Xml2MediumParserException("File " + fileName + " > abstraction " + parent.xmlName + ": there are more than one application-based wrappers for the sub-abstraction " + wrappedAbstraction.xmlName)
            }
            wrappers[0].setWrappedAbstraction(wrappedAbstraction)
            if (wrappedAbstraction.instanceName.get() == "@") {
                wrappers[0].addAnonymous(wrappedAbstraction)
            }
        } else {
            // for anonymous objects we need to traverse applications
            var dereferenced = false
            for (boundAttribute in boundAttributes!!) {
                dereferenced = dereferenced || dereferenceApplicationsForAnonymous(
                    boundAttribute,
                    wrappedAbstraction
                )
                if (dereferenced == true) {
                    boundAttribute.addAnonymous(wrappedAbstraction)
                    break
                }
            }
            if (!dereferenced) {
                throw Xml2MediumParserException("File " + fileName + " > abstraction " + parent.xmlName + ": could not dereference the anonymous abstraction (no reference is present in application) " + wrappedAbstraction.xmlName)
            }
        }
    }

    @Throws(Xml2MediumParserException::class)
    private fun dereferenceApplicationsForAnonymous(
        root: EOApplication,
        wrappedAbstraction: EOAbstraction
    ): Boolean {
        val wrappedName = wrappedAbstraction.xmlName
        if (root.appliedObject == wrappedName) {
            root.setWrappedAbstraction(wrappedAbstraction)
            return true
        }
        val arguments = root.arguments
        for (argument in arguments) {
            if (argument.appliedObject == wrappedName) {
                argument.setWrappedAbstraction(wrappedAbstraction)
                return true
            }
        }
        if (root.isDotNotation) {
            val dotNotationBase = root.dotNotationBase
            if (dotNotationBase != null) {
                if (dotNotationBase.appliedObject == wrappedName) {
                    dotNotationBase.setWrappedAbstraction(wrappedAbstraction)
                    return true
                }
            }
        }

        // let's try to find the wrapper recursively
        for (argument in arguments) {
            if (dereferenceApplicationsForAnonymous(argument, wrappedAbstraction)) {
                return true
            }
        }
        return if (root.isDotNotation) {
            dereferenceApplicationsForAnonymous(root.dotNotationBase!!, wrappedAbstraction)
        } else false
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseGlobalApplications(
        file: File,
        doc: Document,
        xPath: XPath
    ): ArrayList<EOAbstraction> {
        val abstractions = ArrayList<EOAbstraction>()
        val applicationDeclarations: NodeList
        applicationDeclarations = try {
            xPath.evaluate(
                "/program/objects/o[./o[@base and not(@name)]]",
                doc,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("Internal error occurred while parsing global application declarations in File " + file.name + ".")
        }
        for (i in 0 until applicationDeclarations.length) {
            val applicationDeclaration = applicationDeclarations.item(i)
            // retrieving the number of the line
            val lineNumber = parseLineNumber(file.name, applicationDeclaration)
            // retrieving the name attr (which contains a special name with hierarchy encoded into it)
            val xmlName = parseAbstractionXMLName(file.name, lineNumber, applicationDeclaration)
            // retrieving the original-name attr (which contains the normal name)
            val name = parseAbstractionName(file.name, lineNumber, applicationDeclaration)
            // applications cannot have free attributes, no need for parsing here
            val attributes = ArrayList<EOInputAttribute>()
            val abstraction = EOAbstraction(xmlName, name, attributes)
            // we need to transform the tag to make it parsable
            try {
                val body =
                    xPath.evaluate("./o", applicationDeclaration, XPathConstants.NODE) as Element
                body.setAttribute(NAME_ATTR, "@")
            } catch (e: Exception) {
                throw Xml2MediumParserException("Internal error occurred while parsing global application '" + xmlName + "' declaration's body in File " + file.name + ".")
            }
            // now, we parse the body of the global application as if it were application-based decoratee
            val applications = parseApplications(file, applicationDeclaration, xPath, abstraction)
            abstraction.setApplications(applications)
            abstractions.add(abstraction)
        }
        return abstractions
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseAbstractions(
        file: File,
        doc: Document,
        xPath: XPath
    ): ArrayList<EOAbstraction> {
        val abstractions = ArrayList<EOAbstraction>()
        val abstractionDeclarations: NodeList
        abstractionDeclarations = try {
            xPath.evaluate(
                "/program/objects/o[./o[not(@base) or @name]]",
                doc,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("Internal error occurred while parsing abstraction declarations in File " + file.name + ".")
        }
        for (i in 0 until abstractionDeclarations.length) {
            val abstractionDeclaration = abstractionDeclarations.item(i)
            // retrieving the number of the line
            val lineNumber = parseLineNumber(file.name, abstractionDeclaration)
            // retrieving the name attr (which contains a special name with hierarchy encoded into it)
            val xmlName = parseAbstractionXMLName(file.name, lineNumber, abstractionDeclaration)
            // retrieving the original-name attr (which contains the normal name)
            val name = parseAbstractionName(file.name, lineNumber, abstractionDeclaration)
            // retrieving free attributes (if any is present)
            val attributes =
                parseAbstractionFreeAttributes(file.name, lineNumber, abstractionDeclaration, xPath)
            val abstraction = EOAbstraction(xmlName, name, attributes)
            val applications = parseApplications(file, abstractionDeclaration, xPath, abstraction)
            abstraction.setApplications(applications)
            abstractions.add(abstraction)
        }
        return abstractions
    }

    /**
     * Parses the 'line' attribute that contains the line number on which the `declaration` occurred.
     */
    @Throws(Xml2MediumParserException::class)
    private fun parseLineNumber(
        filename: String,
        declaration: Node
    ): String {
        val declarationAttributes = declaration.attributes
        val lineNumberNode =
            declarationAttributes.getNamedItem(LINE_ATTR)
                ?: throw Xml2MediumParserException("File $filename: one of the declarations in the <objects> tag does not contain the required 'line' attribute.")
        return lineNumberNode.nodeValue
    }

    /**
     * Parses the 'name' attribute that contains the name of the abstraction `declaration` with a special structure reflecting objects hierarchy.
     */
    @Throws(Xml2MediumParserException::class)
    private fun parseAbstractionXMLName(
        filename: String,
        lineNumber: String,
        declaration: Node
    ): String {
        val declarationAttributes = declaration.attributes
        val xmlNameNode =
            declarationAttributes.getNamedItem(NAME_ATTR)
                ?: throw Xml2MediumParserException("File $filename, line #$lineNumber: abstraction declaration does not contain the required 'name' attribute.")
        return xmlNameNode.nodeValue
    }

    /**
     * Parses the 'original-name' attribute that contains the normal name (i.e. in the form it is present in the source program)
     * of the abstraction `declaration`.
     */
    @Throws(Xml2MediumParserException::class)
    private fun parseAbstractionName(
        filename: String,
        lineNumber: String,
        declaration: Node
    ): Optional<String> {
        val declarationAttributes = declaration.attributes
        val nameNode = declarationAttributes.getNamedItem(ORIGINAL_NAME_ATTR)
        // it is optional. anonymous objects don't have the original-name
        val name: Optional<String>
        name = if (nameNode == null) {
            Optional.empty()
        } else {
            Optional.of(nameNode.nodeValue)
        }
        return name
    }

    /**
     * Parses the free attributes of the abstraction `declaration`.
     */
    @Throws(Xml2MediumParserException::class)
    private fun parseAbstractionFreeAttributes(
        filename: String,
        lineNumber: String,
        abstractionDeclaration: Node,
        xPath: XPath
    ): ArrayList<EOInputAttribute> {
        val attributes = ArrayList<EOInputAttribute>()
        val attributesDeclarations: NodeList
        attributesDeclarations = try {
            xPath.evaluate(
                "o[not(@base or @as or @level) and @name]",
                abstractionDeclaration,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("File $filename, line #$lineNumber: internal error occurred while parsing free attributes of the abstraction.")
        }
        for (i in 0 until attributesDeclarations.length) {
            val attributeDeclaration = attributesDeclarations.item(i)
            val declarationAttributes = attributeDeclaration.attributes
            // retrieving the name of the attribute
            val nameNode = declarationAttributes.getNamedItem(
                NAME_ATTR
            )
                ?: throw Xml2MediumParserException("File $filename, line #$lineNumber: free attribute declaration inside abstraction declaration does not contain the required 'name' attribute.")
            val name = nameNode.nodeValue
            // retrieving information if the attribute is variable-length
            val varargNode = declarationAttributes.getNamedItem(
                VARARG_ATTR
            )
            var vararg: Boolean
            vararg = varargNode != null
            attributes.add(EOInputAttribute(name, vararg))
        }
        return attributes
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseApplications(
        file: File,
        abstractionDeclaration: Node,
        xPath: XPath,
        baseAbstraction: EOAbstraction
    ): ArrayList<EOApplication> {
        val applications = ArrayList<EOApplication>()
        val applicationDeclarations: NodeList
        applicationDeclarations = try {
            xPath.evaluate(
                "o[not(@as or @level) and @name and @base]",
                abstractionDeclaration,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("Internal error occurred while parsing application declarations in File " + file.name + " for abstraction + " + baseAbstraction.xmlName + ".")
        }
        for (i in 0 until applicationDeclarations.length) {
            val applicationDeclaration = applicationDeclarations.item(i)
            val application = parseApplicationRecursively(
                file.name,
                xPath,
                applicationDeclaration,
                baseAbstraction
            )
            applications.add(application)
        }
        return applications
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseApplicationRecursively(
        fileName: String,
        xPath: XPath,
        application: Node,
        scope: EOAbstraction
    ): EOApplication {
        val lineNumber = parseLineNumber(fileName, application)
        var base = parseApplicationBase(fileName, lineNumber, application)
        val isDotNotation = parseHasMethodAttr(application) || base.startsWith(".")
        val data = parseData(fileName, application, lineNumber)
        if (isDotNotation) {
            // TODO what if base is aliased somehow? need some analysis here
            base = base.replace("\\.".toRegex(), "")
        }
        val name = parseApplicationName(application)
        val eoApplication = EOApplication(isDotNotation, base, name, data)
        eoApplication.scope = scope
        val arguments = ArrayList<EOApplication>()
        val argumentsDeclarations: NodeList
        argumentsDeclarations = try {
            xPath.evaluate(
                "o[not(@as or @level) and @base]",
                application,
                XPathConstants.NODESET
            ) as NodeList
        } catch (e: Exception) {
            throw Xml2MediumParserException("Internal error occurred while parsing application argument declarations in File $fileName at line + $lineNumber.")
        }
        for (i in 0 until argumentsDeclarations.length) {
            val applicationDeclaration = argumentsDeclarations.item(i)
            val argumentApplication =
                parseApplicationRecursively(fileName, xPath, applicationDeclaration, scope)
            arguments.add(argumentApplication)
        }
        var dotNotationBase: EOApplication? = null
        if (isDotNotation) {
            dotNotationBase = arguments[0]
            arguments.removeAt(0)
        }
        eoApplication.dotNotationBase = dotNotationBase
        eoApplication.arguments = arguments
        return eoApplication
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseData(
        fileName: String,
        application: Node,
        lineNumber: String
    ): Optional<EOData> {
        val declarationAttributes = application.attributes
        val dataNode = declarationAttributes.getNamedItem(DATA_ATTR)
        return if (dataNode == null) {
            Optional.empty()
        } else {
            val type = dataNode.nodeValue
            if (type == "array") {
                return Optional.empty() // arrays are handled in another way
            }
            var value: String? = null
            try {
                value = application.firstChild.nodeValue
                when (type) {
                    "int" -> Optional.of(EOint(value.toLong()))
                    "float" -> Optional.of(EOfloat(value.toDouble()))
                    "bool" -> Optional.of(EObool(java.lang.Boolean.parseBoolean(value)))
                    "char" -> Optional.of(
                        EOchar(
                            value[0]
                        )
                    )
                    "string" -> Optional.of(EOstring(value))
                    else -> throw Xml2MediumParserException("File $fileName > line #$lineNumber: unknown data type '$type'")
                }
            } catch (e: Exception) {
                throw Xml2MediumParserException("File $fileName > line #$lineNumber: could not cast '$value' to type '$type'")
            }
        }
    }

    /**
     * Parses the 'name' attribute that contains the normal name (i.e. in the form it is present in the source program)
     * of the application `declaration`.
     */
    private fun parseApplicationName(declaration: Node): Optional<String> {
        val declarationAttributes = declaration.attributes
        val nameNode = declarationAttributes.getNamedItem(NAME_ATTR)
        // name is optional for application
        val name: Optional<String>
        name = if (nameNode == null) {
            Optional.empty()
        } else {
            Optional.of(nameNode.nodeValue)
        }
        return name
    }

    /**
     * Checks if the `declaration` node has the 'method' attribute.
     */
    private fun parseHasMethodAttr(declaration: Node): Boolean {
        val declarationAttributes = declaration.attributes
        val methodNode = declarationAttributes.getNamedItem(METHOD_ATTR)
        return methodNode != null
    }

    @Throws(Xml2MediumParserException::class)
    private fun parseApplicationBase(
        filename: String,
        lineNumber: String,
        declaration: Node
    ): String {
        val declarationAttributes = declaration.attributes
        val baseNode =
            declarationAttributes.getNamedItem(BASE_ATTR)
                ?: throw Xml2MediumParserException("File $filename, line #$lineNumber: application declaration does not contain the required 'base' attribute.")
        return baseNode.nodeValue
    }

    /**
     * Version of lastIndexOf that uses regular expressions for searching.
     *
     * @param str    String in which to search for the pattern.
     * @param toFind Pattern to locate.
     * @return The index of the requested pattern, if found; NOT_FOUND (-1) otherwise.
     */
    fun lastIndexOfRegex(str: String?, toFind: String?): Int {
        val pattern = Pattern.compile(toFind)
        val matcher = pattern.matcher(str)
        var lastIndex = -1

        // Search for the given pattern
        while (matcher.find()) {
            lastIndex = matcher.start()
        }
        return lastIndex
    }
}
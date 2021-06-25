package org.eolang.maven.transpiler.xml2medium;

import org.eolang.maven.transpiler.mediumcodemodel.*;
import org.w3c.dom.*;

import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import java.io.File;
import java.util.ArrayList;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ObjectsParsingUtils {
    private static final String NAME_ATTR = "name";
    private static final String ORIGINAL_NAME_ATTR = "original-name";
    private static final String LINE_ATTR = "line";
    private static final String VARARG_ATTR = "vararg";
    private static final String METHOD_ATTR = "method";
    private static final String BASE_ATTR = "base";
    private static final String DATA_ATTR = "data";


    public static ArrayList<EOAbstraction> parseObjects(File file, Document doc, XPath xPath, EOSourceFile fileScope) throws XML2MediumParser.XML2MediumParserException {

        ArrayList<EOAbstraction> abstractions = parseAbstractions(file, doc, xPath);
        abstractions.addAll(parseGlobalApplications(file, doc, xPath));
        abstractions = deflateAbstractions(abstractions, fileScope);

        return abstractions;
    }


    private static ArrayList<EOAbstraction> deflateAbstractions(ArrayList<EOAbstraction> flattenedAbstractions, EOSourceFile fileScope) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOAbstraction> fileLevelAbstractions = new ArrayList<>();

        for (int i = 0; i < flattenedAbstractions.size(); i++) {
            EOAbstraction abstraction = flattenedAbstractions.get(i);
            String xmlName = abstraction.getXmlName();
            /* analyzing components of the xml name (these reflect the hierarchical structure of the abstraction) */
            int index;
            if (!abstraction.getInstanceName().isPresent()) {
                // anonymous abstraction
                index = lastIndexOfRegex(xmlName, "((\\$\\d+)+\\$\\α\\d+)");
            } else {
                // normal named abstraction
                index = xmlName.lastIndexOf('$');
            }
            if (index == -1) {
                // this abstraction is not nested (it is in the file-level scope)
                abstraction.setScope(fileScope);
                fileLevelAbstractions.add(abstraction);
            } else {
                // this abstraction is nested (and we have to find its scope)
                String parentXmlName = xmlName.substring(0, index);
                deflateAbstraction(flattenedAbstractions, abstraction, parentXmlName, fileScope.getFileName());
            }
        }

        return fileLevelAbstractions;
    }

    private static void deflateAbstraction(ArrayList<EOAbstraction> flattenedAbstractions, EOAbstraction nestedAbstraction, String parentXmlName, String fileName) throws XML2MediumParser.XML2MediumParserException {
        Optional<String> name = nestedAbstraction.getInstanceName();
        Optional<EOAbstraction> parent =
                flattenedAbstractions
                        .stream()
                        .filter(abstraction -> abstraction.getXmlName().equals(parentXmlName))
                        .findFirst();
        if (!parent.isPresent()) {
            throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > abstraction " + nestedAbstraction.getXmlName() + ": could not find its parent with the name " + parentXmlName);
        }

        // this abstraction is scoped to its parent
        nestedAbstraction.setScope(parent.get());

        if (!name.isPresent() || name.get().equals("@")) {
            // we do not need to reference
            // anonymous abstractions and abstraction-based decorations
            // as sub-abstractions
            // we only need to dereference the applications referencing them
            dereferenceWrapperApplications(parent.get(), nestedAbstraction, fileName);
        } else {
            // this is an abstraction-based object attribute
            // so, we add it as a sub-abstraction
            parent.get().addSubAbstraction(nestedAbstraction);
            // and dereference the application referencing it
            dereferenceWrapperApplications(parent.get(), nestedAbstraction, fileName);
        }

    }

    private static void dereferenceWrapperApplications(EOAbstraction parent, EOAbstraction wrappedAbstraction, String fileName) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOApplication> boundAttributes = parent.getBoundAttributes();

        if (wrappedAbstraction.getInstanceName().isPresent()) {
            // for named objects we find their sole wrapper
            EOApplication[] wrappers =
                    boundAttributes
                            .stream()
                            .filter(eoApplication -> eoApplication.getAppliedObject().equals(wrappedAbstraction.getXmlName()))
                            .toArray(EOApplication[]::new);

            if (wrappers.length == 0) {
                throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > abstraction " + parent.getXmlName() + ": there is no application-based wrapper for the sub-abstraction " + wrappedAbstraction.getXmlName());
            }
            if (wrappers.length > 1) {
                throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > abstraction " + parent.getXmlName() + ": there are more than one application-based wrappers for the sub-abstraction " + wrappedAbstraction.getXmlName());
            }
            wrappers[0].setWrappedAbstraction(wrappedAbstraction);
            if (wrappedAbstraction.getInstanceName().get().equals("@")) {
                wrappers[0].addAnonymous(wrappedAbstraction);
            }
        } else {
            // for anonymous objects we need to traverse applications
            boolean dereferenced = false;

            for (int i = 0; i < boundAttributes.size(); i++) {
                dereferenced = dereferenced || dereferenceApplicationsForAnonymous(boundAttributes.get(i), wrappedAbstraction);

                if (dereferenced == true) {
                    boundAttributes.get(i).addAnonymous(wrappedAbstraction);
                    break;
                }
            }

            if (!dereferenced) {
                throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > abstraction " + parent.getXmlName() + ": could not dereference the anonymous abstraction (no reference is present in application) " + wrappedAbstraction.getXmlName());
            }
        }


    }

    private static boolean dereferenceApplicationsForAnonymous(EOApplication root, EOAbstraction wrappedAbstraction) throws XML2MediumParser.XML2MediumParserException {
        String wrappedName = wrappedAbstraction.getXmlName();
        if (root.getAppliedObject().equals(wrappedName)) {
            root.setWrappedAbstraction(wrappedAbstraction);
            return true;
        }
        ArrayList<EOApplication> arguments = root.getArguments();

        for (int i = 0; i < arguments.size(); i++) {
            EOApplication argument = arguments.get(i);

            if (argument.getAppliedObject().equals(wrappedName)) {
                argument.setWrappedAbstraction(wrappedAbstraction);
                return true;
            }
        }

        if (root.isDotNotation()) {
            EOApplication dotNotationBase = root.getDotNotationBase();
            if (dotNotationBase.getAppliedObject().equals(wrappedName)) {
                dotNotationBase.setWrappedAbstraction(wrappedAbstraction);
                return true;
            }
        }

        // let's try to find the wrapper recursively
        for (int i = 0; i < arguments.size(); i++) {
            EOApplication argument = arguments.get(i);
            if (dereferenceApplicationsForAnonymous(argument, wrappedAbstraction)) {
                return true;
            }
        }
        if (root.isDotNotation()) {
            return dereferenceApplicationsForAnonymous(root.getDotNotationBase(), wrappedAbstraction);
        }

        return false;

    }

    private static ArrayList<EOAbstraction> parseGlobalApplications(File file, Document doc, XPath xPath) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOAbstraction> abstractions = new ArrayList<>();
        NodeList applicationDeclarations;
        try {
            applicationDeclarations = (NodeList) xPath.evaluate
                    (
                            "/program/objects/o[./o[@base and not(@name)]]",
                            doc,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing global application declarations in File " + file.getName() + ".");
        }

        for (int i = 0; i < applicationDeclarations.getLength(); i++) {
            Node applicationDeclaration = applicationDeclarations.item(i);
            /* retrieving the number of the line */
            String lineNumber = parseLineNumber(file.getName(), applicationDeclaration);
            /* retrieving the name attr (which contains a special name with hierarchy encoded into it) */
            String xmlName = parseAbstractionXMLName(file.getName(), lineNumber, applicationDeclaration);
            /* retrieving the original-name attr (which contains the normal name) */
            Optional<String> name = parseAbstractionName(file.getName(), lineNumber, applicationDeclaration);
            /* applications cannot have free attributes, no need for parsing here */
            ArrayList<EOInputAttribute> attributes = new ArrayList<>();


            EOAbstraction abstraction = new EOAbstraction(xmlName, name, attributes);
            /* we need to transform the tag to make it parsable */
            try {
                Element body = (Element) xPath.evaluate("./o", applicationDeclaration, XPathConstants.NODE);
                body.setAttribute(NAME_ATTR, "@");
            } catch (Exception e) {
                throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing global application '" + xmlName + "' declaration's body in File " + file.getName() + ".");
            }
            // now, we parse the body of the global application as if it were application-based decoratee
            ArrayList<EOApplication> applications = parseApplications(file, applicationDeclaration, xPath, abstraction);


            abstraction.setApplications(applications);
            abstractions.add(abstraction);
        }


        return abstractions;
    }

    private static ArrayList<EOAbstraction> parseAbstractions(File file, Document doc, XPath xPath) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOAbstraction> abstractions = new ArrayList<>();
        NodeList abstractionDeclarations;
        try {
            abstractionDeclarations = (NodeList) xPath.evaluate
                    (
                            "/program/objects/o[./o[not(@base) or @name]]",
                            doc,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing abstraction declarations in File " + file.getName() + ".");
        }

        for (int i = 0; i < abstractionDeclarations.getLength(); i++) {
            Node abstractionDeclaration = abstractionDeclarations.item(i);
            /* retrieving the number of the line */
            String lineNumber = parseLineNumber(file.getName(), abstractionDeclaration);
            /* retrieving the name attr (which contains a special name with hierarchy encoded into it) */
            String xmlName = parseAbstractionXMLName(file.getName(), lineNumber, abstractionDeclaration);
            /* retrieving the original-name attr (which contains the normal name) */
            Optional<String> name = parseAbstractionName(file.getName(), lineNumber, abstractionDeclaration);
            /* retrieving free attributes (if any is present) */
            ArrayList<EOInputAttribute> attributes = parseAbstractionFreeAttributes(file.getName(), lineNumber, abstractionDeclaration, xPath);

            EOAbstraction abstraction = new EOAbstraction(xmlName, name, attributes);
            ArrayList<EOApplication> applications = parseApplications(file, abstractionDeclaration, xPath, abstraction);
            abstraction.setApplications(applications);
            abstractions.add(abstraction);
        }


        return abstractions;
    }

    /***
     * Parses the 'line' attribute that contains the line number on which the {@code declaration} occurred.
     */
    private static String parseLineNumber(String filename, Node declaration) throws XML2MediumParser.XML2MediumParserException {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node lineNumberNode = declarationAttributes.getNamedItem(LINE_ATTR);
        if (lineNumberNode == null) {
            throw new XML2MediumParser.XML2MediumParserException("File " + filename + ": one of the declarations in the <objects> tag does not contain the required 'line' attribute.");
        }
        String lineNumber = lineNumberNode.getNodeValue();

        return lineNumber;
    }

    /***
     * Parses the 'name' attribute that contains the name of the abstraction {@code declaration} with a special structure reflecting objects hierarchy.
     */
    private static String parseAbstractionXMLName(String filename, String lineNumber, Node declaration) throws XML2MediumParser.XML2MediumParserException {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node xmlNameNode = declarationAttributes.getNamedItem(NAME_ATTR);
        if (xmlNameNode == null) {
            throw new XML2MediumParser.XML2MediumParserException("File " + filename + ", line #" + lineNumber + ": abstraction declaration does not contain the required 'name' attribute.");
        }
        String xmlName = xmlNameNode.getNodeValue();

        return xmlName;
    }

    /***
     * Parses the 'original-name' attribute that contains the normal name (i.e. in the form it is present in the source program)
     * of the abstraction {@code declaration}.
     */
    private static Optional<String> parseAbstractionName(String filename, String lineNumber, Node declaration) throws XML2MediumParser.XML2MediumParserException {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node nameNode = declarationAttributes.getNamedItem(ORIGINAL_NAME_ATTR);
        // it is optional. anonymous objects don't have the original-name
        Optional<String> name;
        if (nameNode == null) {
            name = Optional.empty();
        } else {
            name = Optional.of(nameNode.getNodeValue());
        }

        return name;
    }

    /***
     * Parses the free attributes of the abstraction {@code declaration}.
     */
    private static ArrayList<EOInputAttribute> parseAbstractionFreeAttributes(String filename, String lineNumber, Node abstractionDeclaration, XPath xPath) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOInputAttribute> attributes = new ArrayList<>();
        NodeList attributesDeclarations;
        try {
            attributesDeclarations = (NodeList) xPath.evaluate
                    (
                            "o[not(@base or @as or @level) and @name]",
                            abstractionDeclaration,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("File " + filename + ", line #" + lineNumber + ": internal error occurred while parsing free attributes of the abstraction.");
        }

        for (int i = 0; i < attributesDeclarations.getLength(); i++) {
            Node attributeDeclaration = attributesDeclarations.item(i);
            NamedNodeMap declarationAttributes = attributeDeclaration.getAttributes();
            /* retrieving the name of the attribute */
            Node nameNode = declarationAttributes.getNamedItem(NAME_ATTR);
            if (nameNode == null) {
                throw new XML2MediumParser.XML2MediumParserException("File " + filename + ", line #" + lineNumber + ": free attribute declaration inside abstraction declaration does not contain the required 'name' attribute.");
            }
            String name = nameNode.getNodeValue();
            /* retrieving information if the attribute is variable-length */
            Node varargNode = declarationAttributes.getNamedItem(VARARG_ATTR);
            boolean vararg;
            vararg = varargNode != null;

            attributes.add(new EOInputAttribute(name, vararg));

        }

        return attributes;
    }

    private static ArrayList<EOApplication> parseApplications(File file, Node abstractionDeclaration, XPath xPath, EOAbstraction baseAbstraction) throws XML2MediumParser.XML2MediumParserException {
        ArrayList<EOApplication> applications = new ArrayList<>();
        NodeList applicationDeclarations;
        try {
            applicationDeclarations = (NodeList) xPath.evaluate
                    (
                            "o[not(@as or @level) and @name and @base]",
                            abstractionDeclaration,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing application declarations in File " + file.getName() + " for abstraction + " + baseAbstraction.getXmlName() + ".");
        }

        for (int i = 0; i < applicationDeclarations.getLength(); i++) {
            Node applicationDeclaration = applicationDeclarations.item(i);
            EOApplication application = parseApplicationRecursively(file.getName(), xPath, applicationDeclaration, baseAbstraction);
            applications.add(application);
        }

        return applications;
    }

    private static EOApplication parseApplicationRecursively(String fileName, XPath xPath, Node application, EOAbstraction scope) throws XML2MediumParser.XML2MediumParserException {
        String lineNumber = parseLineNumber(fileName, application);
        String base = parseApplicationBase(fileName, lineNumber, application);
        boolean isDotNotation = parseHasMethodAttr(application) || base.startsWith(".");
        Optional<EOData> data = parseData(fileName, application, lineNumber);
        if (isDotNotation) {
            // TODO what if base is aliased somehow? need some analysis here
            base = base.replaceAll("\\.", "");
        }

        Optional<String> name = parseApplicationName(application);
        EOApplication eoApplication = new EOApplication(isDotNotation, base, name, data);
        eoApplication.setScope(scope);

        ArrayList<EOApplication> arguments = new ArrayList<>();

        NodeList argumentsDeclarations;
        try {
            argumentsDeclarations = (NodeList) xPath.evaluate
                    (
                            "o[not(@as or @level) and @base]",
                            application,
                            XPathConstants.NODESET
                    );
        } catch (Exception e) {
            throw new XML2MediumParser.XML2MediumParserException("Internal error occurred while parsing application argument declarations in File " + fileName + " at line + " + lineNumber + ".");
        }

        for (int i = 0; i < argumentsDeclarations.getLength(); i++) {
            Node applicationDeclaration = argumentsDeclarations.item(i);
            EOApplication argumentApplication = parseApplicationRecursively(fileName, xPath, applicationDeclaration, scope);
            arguments.add(argumentApplication);
        }

        EOApplication dotNotationBase = null;
        if (isDotNotation) {
            dotNotationBase = arguments.get(0);
            arguments.remove(0);
        }

        eoApplication.setDotNotationBase(dotNotationBase);
        eoApplication.setArguments(arguments);

        return eoApplication;
    }

    private static Optional<EOData> parseData(String fileName, Node application, String lineNumber) throws XML2MediumParser.XML2MediumParserException {
        NamedNodeMap declarationAttributes = application.getAttributes();
        Node dataNode = declarationAttributes.getNamedItem(DATA_ATTR);
        if (dataNode == null) {
            return Optional.empty();
        } else {
            String type = dataNode.getNodeValue();
            if (type.equals("array")) {
                return Optional.empty(); // arrays are handled in another way
            }
            String value = null;
            try {
                value = application.getFirstChild().getNodeValue();
                switch (type) {
                    case "int":
                        return Optional.of(new EOint(Long.parseLong(value)));
                    case "float":
                        return Optional.of(new EOfloat(Double.parseDouble(value)));
                    case "bool":
                        return Optional.of(new EObool(Boolean.parseBoolean(value)));
                    case "char":
                        return Optional.of(new EOchar(value.charAt(0)));
                    case "string":
                        return Optional.of(new EOstring(value));
                    default:
                        throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > line #" + lineNumber + ": unknown data type '" + type + "'");

                }
            } catch (Exception e) {
                throw new XML2MediumParser.XML2MediumParserException("File " + fileName + " > line #" + lineNumber + ": could not cast '" + value + "' to type '" + type + "'");
            }
        }
    }

    /***
     * Parses the 'name' attribute that contains the normal name (i.e. in the form it is present in the source program)
     * of the application {@code declaration}.
     */
    private static Optional<String> parseApplicationName(Node declaration) {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node nameNode = declarationAttributes.getNamedItem(NAME_ATTR);
        // name is optional for application
        Optional<String> name;
        if (nameNode == null) {
            name = Optional.empty();
        } else {
            name = Optional.of(nameNode.getNodeValue());
        }

        return name;
    }

    /***
     * Checks if the {@code declaration} node has the 'method' attribute.
     */
    private static boolean parseHasMethodAttr(Node declaration) {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node methodNode = declarationAttributes.getNamedItem(METHOD_ATTR);
        return methodNode != null;
    }

    private static String parseApplicationBase(String filename, String lineNumber, Node declaration) throws XML2MediumParser.XML2MediumParserException {
        NamedNodeMap declarationAttributes = declaration.getAttributes();
        Node baseNode = declarationAttributes.getNamedItem(BASE_ATTR);
        if (baseNode == null) {
            throw new XML2MediumParser.XML2MediumParserException("File " + filename + ", line #" + lineNumber + ": application declaration does not contain the required 'base' attribute.");
        }
        String base = baseNode.getNodeValue();

        return base;
    }

    /**
     * Version of lastIndexOf that uses regular expressions for searching.
     *
     * @param str    String in which to search for the pattern.
     * @param toFind Pattern to locate.
     * @return The index of the requested pattern, if found; NOT_FOUND (-1) otherwise.
     */
    public static int lastIndexOfRegex(String str, String toFind) {
        Pattern pattern = Pattern.compile(toFind);
        Matcher matcher = pattern.matcher(str);
        int lastIndex = -1;

        // Search for the given pattern
        while (matcher.find()) {
            lastIndex = matcher.start();
        }

        return lastIndex;
    }

}

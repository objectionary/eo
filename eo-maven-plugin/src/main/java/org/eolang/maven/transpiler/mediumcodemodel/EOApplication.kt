package org.eolang.maven.transpiler.mediumcodemodel

import org.ainslec.picocog.PicoWriter
import org.eolang.hse.EOarray
import org.eolang.hse.core.EOObject
import org.eolang.hse.core.EOThunk
import org.eolang.maven.transpiler.medium2target.TranslationCommons
import java.util.*

/**
 * Represents the medium code model for the application operation.
 */
class EOApplication(
    /**
     * Does the application refer to
     * the `appliedObject` as an attribute
     * of the `dotNotationBase` object?
     */
    val isDotNotation: Boolean,
    /**
     * The name of the object being applied
     */
    val appliedObject: String,
    /**
     * The name of the application in the EO source code (may be absent)
     */
    val name: Optional<String>,
    /**
     * If the application copies standard data type object (like int or string) the data and its type are stored here
     */
    private val data: Optional<EOData>,
    /**
     * The base object where the dot-notation based application accesses its attribute `appliedObject`
     * May be absent if the application is plain (i.e., not dot-notation based)
     */
    var dotNotationBase: EOApplication? = null,
    /**
     * Arguments of the application (i.e., objects that are passed into the applied object)
     * May be absent
     */
    var arguments: ArrayList<EOApplication> = ArrayList(),
) : EOSourceEntity() {
    /**
     * The scope in which the application is performed
     * Typically, the scope is the nearest abstraction that the application belongs to
     */
    var scope: EOSourceEntity? = null
    /**
     * The name of the application in the target Java source code (may be absent)
     */
    private var targetName: Optional<String>? = null

    /**
     * If the application is a wrapper for some abstraction, the abstraction is stored here
     */
    var wrappedAbstraction: EOAbstraction? = null
        private set

    /**
     * Anonymous objects that are referenced in the scope of this application.
     */
    private val anonymousObjects = ArrayList<EOAbstraction>()
    fun setWrappedAbstraction(wrappedAbstraction: EOAbstraction) {
        if (this.wrappedAbstraction == null || this.wrappedAbstraction!!.xmlName == wrappedAbstraction.xmlName) {
            this.wrappedAbstraction = wrappedAbstraction
        } else {
            throw RuntimeException("application already has the wrapped abstraction")
        }
    }

    fun addAnonymous(anonymous: EOAbstraction) {
        if (anonymousObjects.stream()
                .anyMatch { a: EOAbstraction -> a.anonymousName == anonymous.anonymousName }
        ) {
            return
        }
        anonymousObjects.add(anonymous)
        anonymous.anonymousName = String.format("anonymous$%d", anonymousObjects.size)
    }

    private fun isAccessible(base: String): EOApplication {
        val scope = scope as EOAbstraction?
        return scope!!.boundAttributes!!.stream()
            .filter { a: EOApplication -> a.targetName?.get() == base }
            .findAny().orElse(null)
    }

    private fun aliasToNormalForm(): String {
        val components = appliedObject.split(".")
        var result = ""
        components.indices.forEach { i ->
            if (i != components.size - 1) {
                result += components[i]+"."
            } else {
                result += "EO"+components[i]
            }
        }
        return result
    }// recursive reference

    // some outer object with a fully qualified name
    private val correctReference: String
        private get() {
            if (appliedObject.contains(".")) {
                return if (appliedObject == "org.eolang.array") {
                    String.format("new %s", EOarray::class.java.simpleName)
                } else String.format("new %s", aliasToNormalForm())
                // some outer object with a fully qualified name
            }
            var abstractionScope = scope as EOAbstraction?
            if (abstractionScope!!.xmlName == appliedObject) {
                // recursive reference
                return String.format("new %s", abstractionScope.targetName!!.get())
            }
            val attr = abstractionScope.freeAttributes.stream()
                .filter { a: EOInputAttribute -> a.name == appliedObject }
                .findAny()
            if (attr.isPresent) {
                return String.format("this.%s", attr.get().targetName)
            }
            val application = abstractionScope.boundAttributes!!.stream()
                .filter { a: EOApplication -> a.name.orElse("") == appliedObject }
                .findAny()
            if (application.isPresent) {
                return String.format("this.%s", application.get().targetName!!.get())
            }
            while (abstractionScope!!.scope !is EOSourceFile) {
                abstractionScope = abstractionScope.scope as EOAbstraction?
            }
            val eoSourceFileScope = abstractionScope.scope as EOSourceFile?
            for (abstraction in eoSourceFileScope!!.objects) {
                if (abstraction.instanceName.orElse("") == appliedObject) {
                    return String.format("new %s", abstraction.targetName!!.get())
                }
            }
            throw RuntimeException(String.format("Cannot find referenced %s.", appliedObject))
        }

    override fun toString(): String {
        return if (name.isPresent) {
            "application " + name.get()
        } else {
            "anonymous application of the object $appliedObject"
        }
    }

    /**
     * Transpiles higher level applications
     * (i.e., transpiles proper wrappers for them and delegates the rest to a recursive method)
     */
    override fun transpile(w: PicoWriter): ArrayList<EOTargetFile?>? {
        if (!targetName!!.isPresent) {
            // unnamed applications (i.e. nested)
            // these do not need any wrapping methods for them
            transpileApplication(w)
        } else {
            // bound attribute
            transpileWrapperForBoundAttribute(w)
        }
        return ArrayList()
    }

    // transpiles to a Java method
    private fun transpileWrapperForBoundAttribute(w: PicoWriter) {
        val methodName: String
        val cachedFieldName: String
        if (name.get() == "@") {
            // decoration
            methodName = "_decoratee"
            cachedFieldName = "_cachedDecoratee"
            transpileCachingField(w, cachedFieldName)
            TranslationCommons.bigComment(w, "Declares the decoratee of this object.")
            w.writeln("@Override")
        } else {
            // bound attribute of some type
            methodName = targetName!!.get()
            cachedFieldName = "_cached$methodName"
            transpileCachingField(w, cachedFieldName)
        }
        val isDecoratee = name.get() == "@"
        if (wrappedAbstraction != null) {
            // abstraction-based bound attribute
            if (!isDecoratee) {
                TranslationCommons.bigComment(
                    w,
                    String.format("Abstraction-based bound attribute object '%s'", name.get())
                )
            }
            w.writeln_r(
                String.format(
                    "%s %s %s(%s) {",
                    if (isDecoratee) "protected" else "public",
                    EOObject::class.java.simpleName,
                    methodName,
                    wrappedAbstraction!!.argsString
                )
            )
            if (anonymousObjects.size > 0) {
                TranslationCommons.bigComment(
                    w,
                    "Anonymous objects used in the scope of this method"
                )
                for (anonymous in anonymousObjects) {
                    anonymous.transpile(w)
                }
            }
            if (wrappedAbstraction!!.instanceName.get() == "@") {
                // anonymous-abstraction based decoratee (special case)
                transpileCachingRoutine(
                    w,
                    cachedFieldName
                ) {
                    w.write(
                        String.format(
                            "new %s(%s)",
                            wrappedAbstraction!!.anonymousName,
                            wrappedAbstraction!!.argsString
                        )
                    )
                }
            } else {
                transpileCachingRoutine(
                    w,
                    cachedFieldName
                ) {
                    w.write(
                        String.format(
                            "new %s(%s)",
                            wrappedAbstraction!!.targetName!!.get(),
                            wrappedAbstraction!!.getArgsString(false)
                        )
                    )
                }
            }
        } else {
            // application based bound attribute
            if (!isDecoratee) {
                TranslationCommons.bigComment(
                    w,
                    String.format("Application-based bound attribute object '%s'", name.get())
                )
            }
            w.writeln_r(
                String.format(
                    "%s %s %s() {",
                    if (isDecoratee) "protected" else "public",
                    EOObject::class.java.simpleName,
                    methodName
                )
            )
            if (anonymousObjects.size > 0) {
                TranslationCommons.bigComment(
                    w,
                    "Anonymous objects used in the scope of this method"
                )
                for (anonymous in anonymousObjects) {
                    anonymous.transpile(w)
                }
            }
            transpileCachingRoutine(w, cachedFieldName) { transpileApplication(w) }
        }
        w.writeln_l("}")
    }

    private fun transpileCachingField(w: PicoWriter, cachedFieldName: String) {
        if (wrappedAbstraction != null && !wrappedAbstraction!!.freeAttributes.isEmpty()) {
            // for bound abstractions, no caching is performed
        } else {
            w.writeln()
            TranslationCommons.bigComment(
                w,
                String.format("Field for caching the '%s' attribute.", name.get())
            )
            w.writeln(String.format("private EOObject %s = null;", cachedFieldName))
        }
    }

    private fun transpileCachingRoutine(
        w: PicoWriter,
        cachedFieldName: String,
        returnExpression: Runnable
    ) {
        if (wrappedAbstraction != null && !wrappedAbstraction!!.freeAttributes.isEmpty()) {
            // for bound abstractions, no caching is performed
            w.write("return ")
            returnExpression.run()
            w.writeln(";")
        } else {
            w.writeln_r(String.format("if (%s == null) {", cachedFieldName))
            w.write(String.format("%s = ", cachedFieldName))
            returnExpression.run()
            w.writeln(";")
            w.writeln_l("}")
            w.writeln(String.format("return %s;", cachedFieldName))
        }
    }

    private fun transpileApplication(w: PicoWriter) {
        // anonymous-abstraction based application
        if (!name.isPresent && wrappedAbstraction != null) {
            w.write(
                String.format(
                    "new %s(%s)",
                    wrappedAbstraction!!.anonymousName,
                    wrappedAbstraction!!.argsString
                )
            )
            return
        }
        // data stored inside application
        if (data.isPresent) {
            data.get().transpile(w)
            return
        }
        // dot-notation based application
        if (isDotNotation) {
            transpileDotNotationApplication(w)
            return
        }
        // parent access
        if (appliedObject == "^") {
            w.write("_getParentObject()")
            return
        }
        // plain application
        w.write(correctReference)
        w.write("(")
        if (arguments.size > 0) {
            transpileArgs(w)
        }
        w.write(")")
    }

    private fun transpileDotNotationApplication(w: PicoWriter) {
        w.write("(")
        dotNotationBase?.transpileApplication(w)
        if (appliedObject == "^") {
            w.write(")._getParentObject()")
        } else {
            w.write(")._getAttribute(")
            w.write(String.format("\"EO%s\"", appliedObject))
            if (arguments.size > 0) {
                w.write(", ")
                transpileArgs(w)
            }
            w.write(")")
        }
    }

    private fun transpileArgs(w: PicoWriter) {
        for (i in arguments.indices) {
            val arg = arguments[i]
            w.write(String.format("new %s(() -> (", EOThunk::class.java.simpleName))
            arg.transpileApplication(w)
            w.write("))")
            if (i != arguments.size - 1) {
                w.write(", ")
            }
        }
    }

    init {
        targetName = if (name.isPresent) {
            Optional.of("EO" + name.get())
        } else {
            Optional.empty()
        }
    }
}
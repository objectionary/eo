package org.eolang.maven.transpiler.mediumcodemodel

import com.google.googlejavaformat.java.Formatter
import com.google.googlejavaformat.java.FormatterException
import org.ainslec.picocog.PicoWriter
import org.eolang.hse.EOarray
import org.eolang.hse.core.EOObject
import org.eolang.maven.transpiler.medium2target.TranslationCommons
import java.util.*

class EOAbstraction(
    val xmlName: String,
    val instanceName: Optional<String>,
    val freeAttributes: ArrayList<EOInputAttribute>
) : EOSourceEntity() {
    val subAbstractions: ArrayList<EOAbstraction> = ArrayList()
    var targetName: Optional<String>? = null
    var boundAttributes: ArrayList<EOApplication>? = null
        private set
    var scope: EOSourceEntity? = null
    var anonymousName: String? = null
    fun setApplications(applications: ArrayList<EOApplication>?) {
        boundAttributes = applications
    }

    fun addSubAbstraction(abstraction: EOAbstraction) {
        subAbstractions.add(abstraction)
    }

    private val scopeType: String?
        private get() {
            if (scope is EOSourceFile) {
                return "package"
            } else if (scope is EOAbstraction) {
                return if (!instanceName.isPresent || instanceName.get() == "@") {
                    "anonymous"
                } else {
                    "attribute"
                }
            }
            return null
        }
    private val nestedChain: String
        private get() {
            var scope = scope as EOAbstraction?
            var format = String.format("attribute object '%s' \nof the", instanceName.get())
            while (true) {
                if (scope!!.scopeType == "attribute") {
                    format += String.format(
                        " attribute object '%s' \nof the",
                        scope.instanceName.get()
                    )
                    scope = scope.scope as EOAbstraction?
                } else if (scope.anonymousName != null) {
                    format += String.format(
                        " anonymous object with an assigned pseudo-name '%s'",
                        scope.anonymousName
                    )
                    break
                } else {
                    format += String.format(
                        " package-scope object '%s'",
                        scope.instanceName.get()
                    )
                    break
                }
            }
            return format
        }
    val argsString: String
        get() = getArgsString(true)

    fun getArgsString(useTypes: Boolean): String {
        var result = ""
        for (i in freeAttributes.indices) {
            val attr = freeAttributes[i]
            var type = ""
            if (useTypes) {
                type = EOObject::class.java.simpleName
            }
            if (attr.isVararg) {
                type = "$type..."
            }
            if (useTypes) {
                type += " "
            }
            result += String.format("%s%s", type, attr.targetName)
            if (i != freeAttributes.size - 1) {
                result += ", "
            }
        }
        return result
    }

    override fun toString(): String {
        return if (instanceName.isPresent) {
            "abstraction " + instanceName.get()
        } else {
            "anonymous abstraction"
        }
    }

    override fun transpile(parentWriter: PicoWriter): ArrayList<EOTargetFile?>? {
        return if (scopeType == "package") {
            val file = scope as EOSourceFile?
            val w = PicoWriter()
            transpileFileHeader(w, file)
            transpileClass(w)
            val result = ArrayList<EOTargetFile?>()
            val unformattedCode = w.toString()
            val formattedJava: String
            formattedJava = try {
                Formatter().formatSource(unformattedCode)
            } catch (e: FormatterException) {
                throw RuntimeException("Can't format the output")
            }
            result.add(EOTargetFile(String.format("%s.java", targetName!!.get()), formattedJava))
            result
        } else {
            transpileClass(parentWriter)
            ArrayList()
        }
    }

    /***
     * Transpiles the header of the target Java file (i.e. package and import statements)
     */
    private fun transpileFileHeader(w: PicoWriter, file: EOSourceFile?) {
        // package statement
        w.writeln(String.format("package %s;", file!!.eoPackage.packageName))
        w.writeln("")
        // import the base language objects
        w.writeln(String.format("import org.eolang.hse.*;"))
        w.writeln(String.format("import org.eolang.hse.core.*;"))
        w.writeln(String.format("import java.util.function.Supplier;"))
        w.writeln("")
    }

    private fun transpileClass(w: PicoWriter) {
        if (scopeType == "package") {
            TranslationCommons.bigComment(
                w,
                String.format("Package-scope object '%s'.", instanceName.get())
            )
            w.writeln_r(
                String.format(
                    "public class %s extends %s {",
                    targetName!!.get(),
                    EOObject::class.java.simpleName
                )
            )
        } else if (scopeType == "attribute") {
            TranslationCommons.bigComment(
                w,
                *"$nestedChain.".replaceFirst("a".toRegex(), "A").split("\n").toTypedArray()
            )
            if (scope is EOAbstraction && (scope as EOAbstraction?)!!.anonymousName != null) {
                w.writeln_r(
                    String.format(
                        "class %s extends %s {",
                        targetName!!.get(),
                        EOObject::class.java.simpleName
                    )
                )
            } else {
                w.writeln_r(
                    String.format(
                        "private class %s extends %s {",
                        targetName!!.get(),
                        EOObject::class.java.simpleName
                    )
                )
            }
        } else {
            TranslationCommons.bigComment(
                w,
                String.format("Anonymous object with an assigned pseudo-name '%s'.", anonymousName)
            )
            w.writeln_r(
                String.format(
                    "class %s extends %s {",
                    anonymousName,
                    EOObject::class.java.simpleName
                )
            )
        }
        transpileClassContents(w)
        w.writeln_l("}")
        w.writeln("")
    }

    private fun transpileClassContents(w: PicoWriter) {
        if (freeAttributes.size > 0) {
            w.writeln("")
            transpileFreeAttributes(w)
        }
        transpileConstructor(w)
        transpileParentObject(w)
        transpileDecoratee(w)
        transpileFreeAttrsGetters(w)
        if (boundAttributes!!.size > 0) {
            transpileApplications(w)
        }
        if (subAbstractions.size > 0) {
            transpileSubAbstractions(w)
        }
    }

    private fun transpileFreeAttributes(w: PicoWriter) {
        for (attr in freeAttributes) {
            attr.transpile(w)
        }
    }

    private fun transpileConstructor(w: PicoWriter) {
        w.writeln("")
        val commentParams = ArrayList<String>()
        when (scopeType) {
            "package" -> commentParams.add(
                String.format(
                    "Constructs (via one-time-full application) the package-scope object '%s'.",
                    instanceName.get()
                )
            )
            "attribute" -> commentParams.addAll(
                Arrays.asList(
                    *String.format(
                        "Constructs (via one-time-full application) the %s.",
                        nestedChain
                    ).split("\n").toTypedArray()
                )
            )
            "anonymous" -> commentParams.add(
                String.format(
                    "Constructs (via one-time-full application) the anonymous object with the pseudo-name '%s'.",
                    anonymousName
                )
            )
        }
        if (freeAttributes.size > 0) {
            for (attr in freeAttributes) {
                commentParams.add(
                    String.format(
                        "@param %s the object to bind to the %s.",
                        attr.targetName,
                        attr.description
                    )
                )
            }
        }
        val sArray = arrayOfNulls<String>(commentParams.size)
        TranslationCommons.bigComment(w, *commentParams.toArray(sArray))
        if (scopeType == "anonymous") {
            w.write(String.format("public %s(", anonymousName))
        } else {
            w.write(String.format("public %s(", targetName!!.get()))
        }
        if (freeAttributes.size == 0) {
            w.writeln(") {}")
        } else {
            w.write(argsString)
            w.writeln_r(") {")
            for (attr in freeAttributes) {
                var wrapper: String
                wrapper = if (attr.isVararg) {
                    String.format("new %s(%s)", EOarray::class.java.simpleName, attr.targetName)
                } else {
                    attr.targetName
                }
                w.writeln(String.format("this.%s = %s;", attr.targetName, wrapper))
            }
            w.writeln_l("}")
        }
        w.writeln("")
    }

    private fun transpileFreeAttrsGetters(w: PicoWriter) {
        if (freeAttributes.size > 0) {
            for (attr in freeAttributes) {
                val type =
                    if (attr.isVararg) EOarray::class.java.simpleName else EOObject::class.java.simpleName
                TranslationCommons.bigComment(
                    w,
                    String.format(
                        "Returns the object bound to the '%s' input attribute.",
                        attr.name
                    )
                )
                w.writeln_r(String.format("public %s %s() {", type, attr.targetName))
                w.writeln(String.format("return this.%s;", attr.targetName))
                w.writeln_l("}")
                w.writeln("")
            }
        }
    }

    private fun transpileParentObject(w: PicoWriter) {
        if (scopeType != "package") {
            val scope = scope as EOAbstraction?
            if (scope!!.anonymousName != null) {
                TranslationCommons.bigComment(
                    w,
                    String.format(
                        "Declares the parent object '%s' of this object.",
                        scope.anonymousName
                    )
                )
            } else {
                TranslationCommons.bigComment(
                    w,
                    String.format(
                        "Declares the parent object '%s' of this object.",
                        scope.instanceName.get()
                    )
                )
            }
            w.writeln("@Override")
            w.writeln_r(String.format("protected %s _parent() {", EOObject::class.java.simpleName))
            if (scope.anonymousName != null) {
                w.writeln(String.format("return %s.this;", scope.anonymousName))
            } else {
                w.writeln(String.format("return %s.this;", scope.targetName!!.get()))
            }
            w.writeln_l("}")
            w.writeln("")
        }
    }

    private fun transpileDecoratee(w: PicoWriter) {
        val decoratee =
            boundAttributes!!.stream().filter { o: EOApplication -> o.name.orElse("") == "@" }
                .findFirst()
        if (decoratee.isPresent) {
            decoratee.get().transpile(w)
            // remove it to avoid double transcompilation
            boundAttributes!!.remove(decoratee.get())
            w.writeln("")
        }
    }

    private fun transpileSubAbstractions(w: PicoWriter) {
        for (subAbstraction in subAbstractions) {
            subAbstraction.transpile(w)
            w.writeln("")
        }
    }

    private fun transpileApplications(w: PicoWriter) {
        for (boundAttribute in boundAttributes!!) {
            boundAttribute.transpile(w)
            w.writeln("")
        }
    }

    init {
        targetName = if (instanceName.isPresent) {
            Optional.of("EO" + instanceName.get())
        } else {
            Optional.empty()
        }
    }
}
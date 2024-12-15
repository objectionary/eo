/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

/**
 * Check all XSL stylesheets against a predefined set of rules.
 *
 * The rules were created by Mukul Gandhi (gandhi.mukul@gmail.com)
 * and published here: https://gandhimukul.tripod.com/xslt/xslquality.html
 */

import groovy.io.FileType
import groovy.io.FileVisitResult
import groovy.xml.XmlUtil

class MatchRule {

    String xpath
    String warning

}

List<MatchRule> rules = List.of(new MatchRule(
        xpath: '//*[name()="xsl:for-each" or name()="xsl:if" or name()="xsl:when" or name()="xsl:otherwise"]' +
                '[(count(node()) = count(text())) and (normalize-space() = "")]',
        warning: 'Don\'t use empty content for instructions like \'xsl:for-each\' \'xsl:if\' \'xsl:when\' etc.'
), new MatchRule(
        xpath: '/xsl:stylesheet[@version = "2.0"]//@select[contains(., ":node-set")]',
        warning: 'Don\'t use node-set extension function if using XSLT 2.0'
), new MatchRule(
        xpath: '//xsl:function[not(some $x in //(@match | @select) satisfies contains($x, @name))]',
        warning: 'Stylesheet functions are unused'
), new MatchRule(
        xpath: '//xsl:template[@name and not(@match)][not(//xsl:call-template/@name = @name)]',
        warning: 'Named templates in stylesheet are unused'
), new MatchRule(
        xpath: '//xsl:variable[not(some $att in //@* satisfies contains($att, concat("$", @name)))]',
        warning: 'Variable is unused in the stylesheet'
), new MatchRule(
        xpath: '//*[name()="xsl:function" or name()="xsl:template"]/xsl:param[not(some $x in ..//(node() ' +
                '| @*) satisfies contains($x, concat("$", @name)))]',
        warning: 'Function or template parameter is unused in the function/template body'
), new MatchRule(
        xpath: '/xsl:stylesheet/xsl:output[@method = \'xml\']' +
                '[starts-with(//xsl:template[.//html or .//HTML]/@match, "/")]',
        warning: 'Using the output method \'xml\' when generating HTML code'
), new MatchRule(
        xpath: '//@*[contains(., "name(") or contains(., "local-name(")]',
        warning: 'Using name() function when local-name() could be appropriate (and vice-versa)'
), new MatchRule(
        xpath: '//xsl:variable[(count(*) = 1) and (count(xsl:value-of) = 1)]',
        warning: 'Assign value to a variable using the "select" syntax if assigning a string value'
), new MatchRule(
        xpath: '//(@match | @select | @test)[starts-with(., "//")]',
        warning: 'Avoid using the operator // near the root of a large tree'
), new MatchRule(
        xpath: '//(@match | @select | @test)[not(starts-with(., "//")) and contains(., "//")]',
        warning: 'Avoid using the operator // in XPath expressions'
), new MatchRule(
        xpath: '//*[name()="xsl:template" or name()="xsl:function"][count(.//xsl:*) > 50]',
        warning: 'The function or template\'s size/complexity is high. There is need for refactoring the code.'
), new MatchRule(
        xpath: '//xsl:template[starts-with(@match, "")][(count(*) = count(xsl:variable)) ' +
                'and (normalize-space(string-join(text(), "")) = "")]',
        warning: 'The stylesheet is not generating any useful output. Please relook at the stylesheet logic.'
), new MatchRule(
        xpath: '//@*[name()="match" or name()="select"][contains(., "namespace::")][/xsl:stylesheet/@version = "2.0"]',
        warning: 'Using the deprecated namespace axis, when working in XSLT 2.0 mode'
), new MatchRule(
        xpath: '//@*[name()="match" or name()="select"][contains(., "child::") ' +
                'or contains(., "attribute::") or contains(., "parent::node()")]',
        warning: 'Using the lengthy axis specifiers like child::, attribute:: or parent::node()'
), new MatchRule(
        xpath: '//@disable-output-escaping[. = "yes"]',
        warning: 'Have set the disable-output-escaping attribute to \'yes\'. Please relook at the stylesheet logic.'
), new MatchRule(
        xpath: '//xsl:element[not(contains(@name, "$") or (contains(@name, "(") and contains(@name, ")")) ' +
                'or (contains(@name, "{") and contains(@name, "}")))]',
        warning: 'Creating an element node using the xsl:element instruction when could have been possible directly'
), new MatchRule(
        xpath: '//xsl:apply-templates[some $var in ancestor::xsl:template[1]//xsl:variable satisfies (($var << .) ' +
                'and starts-with(@select, $var/@name))]',
        warning: 'You might be confusing a variable reference with a node reference'
), new MatchRule(
        xpath: '//@*[(contains(., "true") and not(contains(., "true()"))) ' +
                'or (contains(., "false") and not(contains(., "false()")))]',
        warning: 'Incorrectly using the boolean constants as \'true\' or \'false\''
), new MatchRule(
        xpath: '//*[name()="xsl:variable" or name()="xsl:template"][string-length(@name) = 1] ' +
                '| //xsl:function[string-length(substring-after(@name, ":")) = 1]',
        warning: 'Using a single character name for variable/function/template. ' +
                'Use meaningful names for these features.'
), new MatchRule(
        xpath: '//*[name()="xsl:variable" or name()="xsl:template"][(string-length(@name) > 1) ' +
                'and matches(@name, "[0-9].+")] | //xsl:function[(string-length(substring-after(@name, ":")) > 1) ' +
                'and matches(substring-after(@name, \':\'), \'[0-9].+\')]',
        warning: 'The variable/function/template name starts with a numeric character'
// commented early:
//), new MatchRule(
//      xpath:'/xsl:stylesheet[count(//xsl:template[@match and not(@name)][count(*) < 3] ) >= 10]',
//      warning:'Too many low granular templates in the stylesheet (10 or more)'
//), new MatchRule(
//      xpath:'/xsl:stylesheet[count(//xsl:template | //xsl:function) = 1]',
//      warning:'Using a single template/function in the stylesheet. You can modularize the code.'
//), new MatchRule(
//      xpath:'/xsl:stylesheet[not(every $s in in-scope-prefixes(.)[not(. = "xml" or . = "")] ' +
//              'satisfies exists(//(*[not(xsl:stylesheet)] | @*[not(parent::xsl:*)] | @select[parent::xsl:*] | @as ' +
//              '| @name[parent::xsl:*])[starts-with(name(), concat($s, ":")) or starts-with(., concat($s, ":"))]))]',
//      warning:'There are redundant namespace declarations in the xsl:stylesheet element'
//), new MatchRule(
//      xpath:'/xsl:stylesheet[@version = "2.0"][not(some $x in .//@* satisfies contains($x, "xs:"))]',
//      warning:'The stylesheet is not using any of the built-in Schema types (xs:string etc.), ' +
//              'when working in XSLT 2.0 mode'
))

int errors = 0
rules.forEach {
    matchRule ->
        String xpath = matchRule.xpath
        String warning = matchRule.warning
        com.jcabi.xml.XSLDocument check = new com.jcabi.xml.XSLDocument("""
    <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    <xsl:output method="text" omit-xml-declaration="yes" indent="no"/>
    <xsl:template match="${XmlUtil.escapeXml(xpath)}">
      <xsl:text>at </xsl:text>
      <xsl:value-of select="name(.)"/>
    </xsl:template>
    <xsl:template match="*|@*">
    </xsl:template>
    </xsl:stylesheet>
    """)
        log.info("${warning}:")
        new File('.').traverse(
                type: FileType.FILES,
                preDir: { file ->
                    if (file.name == 'target') {
                        return FileVisitResult.SKIP_SUBTREE
                    }
                },
                nameFilter: ~/.*\.xsl/,
        ) {
            file ->
                com.jcabi.xml.XMLDocument xsl = new com.jcabi.xml.XMLDocument(file)
                String ret = check.applyTo(xsl)
                if (!ret.empty) {
                    log.info("  ERROR: ${file} (${ret})")
                    ++errors
                }
        }
}
assert errors == 0
true


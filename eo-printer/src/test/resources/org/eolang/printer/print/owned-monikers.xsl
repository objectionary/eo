<?xml version="1.0" encoding="UTF-8"?>
<!-- SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com -->
<!-- SPDX-License-Identifier: MIT -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" exclude-result-prefixes="eo" id="owned-monikers" version="3.0">
  <xsl:import href="/org/eolang/printer/print/merge-monikers.xsl"/>
  <xsl:template match="/">
    <targets const="{eo:kept-const-ref(//o[@name='constref'])/@local}" local="{eo:kept-local-ref(//o[@name='localref'])/@local}"/>
  </xsl:template>
</xsl:stylesheet>

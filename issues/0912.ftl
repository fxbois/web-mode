[#include "/WEB-INF/common/header.ftl"]
[#assign testVar=false]
[#if object?has_content]
  <span class="">Has content</span>
[#else]
  <span class="">No content</span>
[/#if]

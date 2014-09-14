<title>{#title#}</title>
<ul>
  {foreach $cols as $col}
    <li>{$col}</li>
  {/foreach}
</ul>

{* this multiline smarty
   comment is xsxdc
   not sent to browser *}


{if $logged_in}
  <p>Welcome, {$name}</p>
{else}
  <p>Signup</p>
{/if}
<span>{$var->method()}</span>

<ul>
  {foreach $myColors as $color}
    <li>{$color}</li>
  {/foreach}
</ul>

{"="|str_repeat:80}

{$smarty.env.PATH}

{include file="header.tpl" nocache}

{assign var=foo value={$counter}}  // plugin result

{assign var=foo value=substr($bar,2,5)}  // PHP function result

{assign var=foo value=$bar|strlen}  // using modifier

{assign var=foo value=$buh+$bar|strlen}  // more complex expression

{html_select_date display_days=true}

{mailto address="smarty@example.com"}

<select name="company_id">
  {html_options options=$companies selected=$company_id}
</select>

{counter start=0 skip}<br />

{func var="test $foo test"}

{$foo->bar-$bar[1]*$baz->foo->bar()-3*7}

{include file=$includeFile}

{html_options options=$vals selected=$selected_id}

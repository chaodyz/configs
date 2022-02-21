local M ={}

local getSigns = function ()
    return {
        -- Option 1
        { name = "DiagnosticSignError", text = "" },
        { name = "DiagnosticSignWarn", text = "" },
        { name = "DiagnosticSignHint", text = "" },
        { name = "DiagnosticSignInfo", text = "" }

        -- Option 2
        -- { name = "DiagnosticSignError", text = "" },
        -- { name = "DiagnosticSignWarn", text = "" },
        -- { name = "DiagnosticSignHint", text = "" },
        -- { name = "DiagnosticSignInfo", text = "" }
    }
end

local getIcons = function ()
return {
      hint = "",
      info = "",
      warning = "",
      error = "",
    }
end

M.signs  = getSigns();
M.icons = getIcons();
return M


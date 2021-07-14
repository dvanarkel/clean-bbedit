implementation module Eastwood.Pass.BasicValueCAFs

import StdEnv

import Clean.Parse
import Clean.PrettyPrint
from Data.Func import $
import Data.Maybe
from Text import concat5

import qualified syntax
from syntax import qualified :: ExprWithLocalDefs{..}, :: Ident{..}, :: Rhs{..}

import Eastwood.Diagnostic
import Eastwood.Range
import Eastwood.SyntaxTree

DEFAULT_SEVERITY :== Warning

BASIC_VALUE_CAF_CODE :== 0

runPass :: !BasicValueCAFsConfiguration ![String] !ParsedModule HashTable -> [Diagnostic]
runPass config lines mod _ =
	diagnostics severity lines mod
where
	severity = fromMaybe DEFAULT_SEVERITY config.BasicValueCAFsConfiguration.severity

diagnostics :: !DiagnosticSeverity ![String] !ParsedModule -> [Diagnostic]
diagnostics severity lines mod = catMaybes $ map check $ allDefinitions mod
where
	check :: !ParsedDefinition -> ?Diagnostic
	check ('syntax'.PD_Function pos id _ _ {'syntax'.rhs_alts='syntax'.UnGuardedExpr expr} 'syntax'.FK_Caf) =
		case expr.'syntax'.ewl_expr of
			expr=:('syntax'.PE_Basic _) ->
				?Just
					{ range =
						{ start =
							{ line = line
							, character = 1
							}
						, end =
							{ line = line
							, character = size (lines !! (line-1))
							}
						}
					, severity = severity
					, dCode = BASIC_VALUE_CAF_CODE
					, source = BasicValueCAFsPass
					, message = concat5
						"CAF '" id.'syntax'.id_name
						"' with a basic value '" (cpp expr)
						"' would be faster as a normal function or macro"
					}
			_ ->
				?None
	where
		line = case pos of
			'syntax'.LinePos _ line -> line
			_ -> abort "BasicValueCAFs: unexpected Position\n"
	check _ =
		?None

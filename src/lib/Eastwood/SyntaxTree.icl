implementation module Eastwood.SyntaxTree

from Data.List import concatMap

import syntax

instance allDefinitions [a] | allDefinitions a
where
	allDefinitions xs = concatMap allDefinitions xs

instance allDefinitions [!a!] | allDefinitions a
where
	allDefinitions xs = allDefinitions [x \\ x <|- xs]

instance allDefinitions (Optional a) | allDefinitions a
where
	allDefinitions (Yes x) = allDefinitions x
	allDefinitions No = []

instance allDefinitions ParsedModule
where
	allDefinitions {mod_defs} = allDefinitions mod_defs

instance allDefinitions ParsedDefinition
where
	allDefinitions def = [def:recurse def]
	where
		recurse (PD_Function _ _ _ _ _ _) =
			[]
		recurse (PD_NodeDef _ _ _) =
			[]
		recurse (PD_Type _) =
			[]
		recurse (PD_TypeSpec _ _ _ _ _) =
			[]
		recurse (PD_Class _ members) =
			allDefinitions members
		recurse (PD_Instance members) =
			allDefinitions members
		recurse (PD_Instances instances) =
			allDefinitions instances
		recurse (PD_Import _) =
			[]
		recurse (PD_ImportedObjects _) =
			[]
		recurse (PD_ForeignExport _ _ _ _) =
			[]
		recurse (PD_Generic _) =
			[]
		recurse (PD_GenericCase def _) =
			allDefinitions def
		recurse (PD_Derive defs) =
			[]
		recurse (PD_DeriveInstanceMember _ _ _ _ _) =
			[]
		recurse (PD_DeriveFunction _ _ _) =
			[]
		recurse PD_Erroneous =
			[]

instance allDefinitions ParsedInstanceAndMembers
where
	allDefinitions {pim_members} = allDefinitions pim_members

instance allDefinitions GenericCaseDef
where
	allDefinitions {gc_gcf} = allDefinitions gc_gcf

instance allDefinitions GenericCaseFunctions
where
	allDefinitions (GCF _ gcf) = allDefinitions gcf
	allDefinitions (GCFS gcfs) = allDefinitions gcfs
	allDefinitions (GCFC _ _) = []
	allDefinitions (GCFCExcept _ _ _) = []

instance allDefinitions GCF
where
	allDefinitions {gcf_body} = allDefinitions gcf_body

instance allDefinitions GenericCaseBody
where
	allDefinitions (GCB_ParsedBody _ rhs) = allDefinitions rhs
	allDefinitions _ = []

instance allDefinitions Rhs
where
	allDefinitions {rhs_alts,rhs_locals} =
		allDefinitions rhs_alts ++ allDefinitions rhs_locals

instance allDefinitions LocalDefs
where
	allDefinitions (LocalParsedDefs defs) = allDefinitions defs
	allDefinitions (CollectedLocalDefs defs) = allDefinitions defs
	allDefinitions NoCollectedLocalDefs = []

instance allDefinitions OptGuardedAlts
where
	allDefinitions (GuardedAlts alternatives otherwiseAlternative) =
		allDefinitions alternatives ++ allDefinitions otherwiseAlternative
	allDefinitions (UnGuardedExpr expr) =
		allDefinitions expr

instance allDefinitions ExprWithLocalDefs
where
	allDefinitions {ewl_nodes,ewl_expr,ewl_locals} =
		allDefinitions ewl_nodes ++ allDefinitions ewl_expr ++ allDefinitions ewl_locals

instance allDefinitions GuardedExpr
where
	allDefinitions {alt_nodes,alt_guard,alt_expr} =
		allDefinitions alt_nodes ++ allDefinitions alt_guard ++ allDefinitions alt_expr

instance allDefinitions NodeDefWithLocals
where
	allDefinitions {ndwl_def,ndwl_locals} =
		allDefinitions ndwl_def.bind_src ++ allDefinitions ndwl_def.bind_dst ++ allDefinitions ndwl_locals

instance allDefinitions CollectedLocalDefs
where
	allDefinitions {loc_nodes} = allDefinitions loc_nodes

instance allDefinitions (NodeDef ParsedExpr)
where
	allDefinitions {nd_dst,nd_alts,nd_locals} =
		allDefinitions nd_dst ++ allDefinitions nd_alts ++ allDefinitions nd_locals

instance allDefinitions ParsedExpr
where
	allDefinitions (PE_List exprs) =
		allDefinitions exprs
	allDefinitions (PE_Ident _) =
		[]
	allDefinitions (PE_Basic _) =
		[]
	allDefinitions (PE_Bound {bind_src}) =
		allDefinitions bind_src
	allDefinitions (PE_Lambda _ exprs rhs _) =
		allDefinitions exprs ++ allDefinitions rhs
	allDefinitions (PE_Tuple exprs) =
		allDefinitions exprs
	allDefinitions (PE_Record org_record _ fields) =
		allDefinitions org_record ++ allDefinitions [f.bind_src \\ f <- fields]
	allDefinitions (PE_ArrayPattern elems) =
		allDefinitions [[e.bind_src:e.bind_dst] \\ e <- elems]
	allDefinitions (PE_UpdateComprehension org idx new qualifiers) =
		allDefinitions org ++
		allDefinitions idx ++
		allDefinitions new ++
		allDefinitions qualifiers
	allDefinitions (PE_ArrayDenot _ exprs) =
		allDefinitions exprs
	allDefinitions (PE_Selection _ expr selections) =
		allDefinitions expr ++ allDefinitions selections
	allDefinitions (PE_Update org selections new) =
		allDefinitions org ++ allDefinitions selections ++ allDefinitions selections
	allDefinitions (PE_Case _ expr alts) =
		allDefinitions expr ++ allDefinitions alts
	allDefinitions (PE_If _ cond if_true if_false) =
		allDefinitions cond ++ allDefinitions if_true ++ allDefinitions if_false
	allDefinitions (PE_Let locals expr) =
		allDefinitions locals ++ allDefinitions expr
	allDefinitions (PE_ListCompr _ _ expr qualifiers) =
		allDefinitions expr ++ allDefinitions qualifiers
	allDefinitions (PE_ArrayCompr _ expr qualifiers) =
		allDefinitions expr ++ allDefinitions qualifiers
	allDefinitions (PE_Sequ sequence) =
		allDefinitions sequence
	allDefinitions PE_WildCard =
		[]
	allDefinitions (PE_Matches _ expr pattern _) =
		allDefinitions expr ++ allDefinitions pattern
	allDefinitions (PE_QualifiedIdent _ _) =
		[]
	allDefinitions (PE_ABC_Code _ _) =
		[]
	allDefinitions (PE_Any_Code _ _ _) =
		[]
	allDefinitions (PE_DynamicPattern expr _) =
		allDefinitions expr
	allDefinitions (PE_Dynamic expr _) =
		allDefinitions expr
	allDefinitions (PE_Generic _ _) =
		[]
	allDefinitions (PE_TypeSignature _ expr) =
		allDefinitions expr
	allDefinitions PE_Empty =
		[]

instance allDefinitions Qualifier
where
	allDefinitions {qual_generators,qual_let_defs,qual_filter} =
		allDefinitions qual_generators ++ allDefinitions qual_let_defs ++ allDefinitions qual_filter

instance allDefinitions Generator
where
	allDefinitions {gen_pattern,gen_expr} =
		allDefinitions gen_pattern ++ allDefinitions gen_expr

instance allDefinitions Sequence
where
	allDefinitions (SQ_FromThen _ f t) = allDefinitions f ++ allDefinitions t
	allDefinitions (SQ_FromThenTo _ f then to) = allDefinitions f ++ allDefinitions then ++ allDefinitions to
	allDefinitions (SQ_From _ f) = allDefinitions f
	allDefinitions (SQ_FromTo _ f t) = allDefinitions f ++ allDefinitions t

instance allDefinitions ParsedSelection
where
	allDefinitions (PS_Record _ _) = []
	allDefinitions (PS_QualifiedRecord _ _ _) = []
	allDefinitions (PS_Array idx) = allDefinitions idx
	allDefinitions PS_Erroneous = []

instance allDefinitions CaseAlt
where
	allDefinitions {calt_pattern,calt_rhs} =
		allDefinitions calt_pattern ++ allDefinitions calt_rhs

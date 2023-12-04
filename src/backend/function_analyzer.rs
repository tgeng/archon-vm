use std::collections::HashMap;
use crate::ast::term::CTerm;

#[derive(Debug)]
pub struct FunctionAnalyzer {
    pub(crate) count: usize,
    /// The key is the block id of the case block, the value is a tuple of (branch block ids,
    /// default block id, joining block id).
    pub(crate) case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
    pub(crate) has_non_tail_complex_effects: bool,
}

impl FunctionAnalyzer {
    pub(crate) fn new() -> Self {
        Self {
            count: 1,
            case_blocks: HashMap::new(),
            has_non_tail_complex_effects: false,
        }
    }
    pub(crate) fn analyze(&mut self, c_term: &CTerm, is_tail: bool) {
        match c_term {
            CTerm::Redex { function, .. } => self.analyze(function, is_tail),
            CTerm::Force { may_have_complex_effects: true, .. } if !is_tail => {
                self.count += 1;
                self.has_non_tail_complex_effects = true;
            }
            CTerm::Let { t, body, .. } => {
                self.analyze(t, false);
                self.analyze(body, is_tail);
            }
            CTerm::Def { may_have_complex_effects: true, .. } if !is_tail => {
                self.count += 1;
                self.has_non_tail_complex_effects = true;
            }
            CTerm::CaseInt { branches, default_branch, .. } => {
                let current_block_id = self.count - 1;
                let mut branch_block_ids = Vec::new();
                for (_, branch) in branches {
                    branch_block_ids.push(self.count);
                    self.count += 1;
                    self.analyze(branch, is_tail);
                }
                let default_block_id = self.count;
                self.count += 1;
                match default_branch {
                    None => {}
                    Some(default_branch) => self.analyze(default_branch, is_tail),
                }
                let joining_block_id = self.count;
                self.count += 1;
                self.case_blocks.insert(current_block_id, (branch_block_ids, default_block_id, joining_block_id));
            }
            CTerm::OperationCall { complex: true, .. } if !is_tail => {
                self.count += 1;
                self.has_non_tail_complex_effects = true;
            }
            _ => {}
        }
    }
}

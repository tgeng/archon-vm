use std::collections::HashMap;
use crate::ast::term::CTerm;

pub struct FunctionAnalyzer {
    pub(crate) count: usize,
    pub(crate) case_blocks: HashMap<usize, (Vec<usize>, usize, usize)>,
}

impl FunctionAnalyzer {
    pub(crate) fn new() -> Self {
        Self {
            count: 1,
            case_blocks: HashMap::new(),
        }
    }
    pub(crate) fn analyze(&mut self, c_term: &CTerm, is_tail: bool) {
        match c_term {
            CTerm::Redex { function, .. } => self.analyze(function, is_tail),
            CTerm::Force { may_have_complex_effects: true, .. } if !is_tail => self.count += 1,
            CTerm::Let { t, body, .. } => {
                self.analyze(t, false);
                self.analyze(body, is_tail);
            }
            CTerm::Def { may_have_complex_effects: true, .. } if !is_tail => self.count += 1,
            CTerm::CaseInt { branches, default_branch, .. } => {
                let current_block_id = self.count;
                let mut branch_block_ids = Vec::new();
                for (_, branch) in branches {
                    self.count += 1;
                    branch_block_ids.push(self.count);
                    self.analyze(branch, is_tail);
                }
                self.count += 1;
                let default_block_id = self.count;
                match default_branch {
                    None => {}
                    Some(default_branch) => self.analyze(default_branch, is_tail),
                }
                self.count += 1;
                let joining_block_id = self.count;
                self.case_blocks.insert(current_block_id, (branch_block_ids, default_block_id, joining_block_id));
            }
            CTerm::OperationCall { simple: false, .. } if !is_tail => self.count += 1,
            _ => {}
        }
    }
}

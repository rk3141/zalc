const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Token = union(enum) {
    Identifier: []const u8,
    KeywordDefine,
    BracketOpen,
    BracketClose,
    OpPlus,
    OpMinus,
    OpMult,
    OpDiv,
    OpPow,
    Number: f64,

    fn is_oper(self: Token) bool {
        return self == .OpPlus or self == .OpMult or self == .OpMinus or self == .OpDiv or self == .OpPow;
    }

    fn precedence(self: Token) usize {
        return switch (self) {
            .OpMinus, .OpPlus => 1,
            .OpDiv, .OpMult => 10,
            .OpPow => 100,
            else => @panic("what the hell"),
        };
    }
};

fn isalpha(chr: u8) bool {
    return (chr >= 'a' and chr <= 'z') or (chr >= 'A' and chr <= 'Z');
}
fn isalphanum(chr: u8) bool {
    return (chr >= 'a' and chr <= 'z') or (chr >= 'A' and chr <= 'Z') or (chr >= '0' and chr <= '9');
}
const Lexer = struct {
    code: []const u8,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, code: []const u8) Self {
        return .{
            .allocator = allocator,
            .code = code,
        };
    }

    pub fn lex(self: Self) !ArrayList(Token) {
        var token_list = ArrayList(Token).init(self.allocator);
        var i: usize = 0;

        while (i < self.code.len) {
            var c = self.code[i];

            switch (c) {
                '(' => {
                    i += 1;
                    try token_list.append(.BracketOpen);
                },
                ')' => {
                    i += 1;
                    try token_list.append(.BracketClose);
                },
                '+' => {
                    i += 1;
                    try token_list.append(.OpPlus);
                },
                '*' => {
                    i += 1;
                    try token_list.append(.OpMult);
                },
                '-' => {
                    i += 1;
                    try token_list.append(.OpMinus);
                },
                '/' => {
                    i += 1;
                    try token_list.append(.OpDiv);
                },
                '^' => {
                    i += 1;
                    try token_list.append(.OpPow);
                },
                'a'...'z', 'A'...'Z' => {
                    const base = i;
                    while (isalphanum(c)) {
                        i += 1;
                        if (i >= self.code.len) break;
                        c = self.code[i];
                    }

                    const word = self.code[base..i];
                    try token_list.append(Token{ .Identifier = word });
                },
                '0'...'9' => {
                    var num: f64 = 0.0;

                    var decimal: f64 = 1.0;
                    var post_decimal = false;

                    while (c >= '0' and c <= '9') {
                        if (post_decimal) {
                            decimal *= 10;
                        }
                        num *= 10;
                        num += @floatFromInt(c - '0');
                        i += 1;
                        if (i < self.code.len) {
                            c = self.code[i];
                            if (c == '.') {
                                post_decimal = true;
                                i += 1;
                                if (i < self.code.len) {
                                    c = self.code[i];
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    if (post_decimal)
                        num /= decimal;
                    try token_list.append(Token{ .Number = num });
                },
                else => i += 1,
            }
        }
        return token_list;
    }
};

const Tree = struct {
    allocator: Allocator,
    root: ?*Node = null,
    const Node = struct {
        value: Token,
        a: ?*Node,
        b: ?*Node,
        atomic: bool = false,

        fn cleanup(self: *Node, allocator: Allocator) void {
            if (self.a) |a| {
                a.cleanup(allocator);
            }
            if (self.b) |b| {
                b.cleanup(allocator);
            }
            allocator.destroy(self);
        }

        fn print(self: Node) void {
            std.debug.print("{any}\n", .{self.value});
            if (self.a) |a| {
                a.print();
            }
            if (self.b) |b| {
                b.print();
            }
        }

        pub fn eval(self: *Node) f64 {
            if (self.value.is_oper()) {
                return switch (self.value) {
                    .OpPlus => eval(self.a.?) + eval(self.b.?),
                    .OpMult => eval(self.a.?) * eval(self.b.?),
                    .OpMinus => eval(self.a.?) - eval(self.b.?),
                    .OpDiv => eval(self.a.?) / eval(self.b.?),
                    .OpPow => std.math.pow(f64, eval(self.a.?), eval(self.b.?)),

                    else => -69420,
                };
            } else {
                return self.value.Number;
            }
        }
    };

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn parse(self: *Self, tokens: []const Token) !void {
        var i: usize = 0;
        while (i < tokens.len) {
            // std.debug.print("token[{}] = {any}\n", .{ i, tokens[i] });
            switch (tokens[i]) {
                .BracketOpen => {
                    // Tracks the depth of brackets to match them correctly
                    var depth: usize = 1;
                    var j = i;
                    while (j < tokens.len - 1) {
                        j += 1;
                        if (tokens[j] == .BracketClose and depth == 1) {
                            // If the matching bracket is found, collect the tokens between the brackets
                            // And insert them as one root
                            var subtree = Tree.init(self.allocator);
                            // std.debug.print("{any}\n", .{tokens[(i + 1)..j]});
                            // Sort out if there are any other nested brackets, they are simply resolved recursively
                            try subtree.parse(tokens[(i + 1)..j]);
                            // Make them such that they are not affected by operator precedence things caused by further tokens
                            subtree.root.?.atomic = true;
                            // subtree.root.?.print();

                            if (self.root) |root| {
                                if (root.b == null) {
                                    // If the right child is empty, simply place the subtree at the root
                                    root.b = subtree.root;
                                } else {
                                    // If not empty
                                    var walker_null = root.b;

                                    // Walk rightwards and fill the first empty child with the subtree (no idea whats the proper term for this kind of walk)
                                    while (walker_null) |walker| {
                                        if (walker.a == null) {
                                            walker.a = subtree.root;
                                            break;
                                        } else if (walker.b == null) {
                                            walker.b = subtree.root;
                                            break;
                                        } else {
                                            // if none are empty, walk ahead to the right child
                                            walker_null = walker.b;
                                        }
                                    }
                                }
                            } else {
                                // if the bracket-ed expression is the first, simply make it the root
                                const new_root = subtree.root.?;
                                self.root = new_root;
                            }
                            break;
                        }

                        if (tokens[j] == .BracketOpen) depth += 1;
                        if (tokens[j] == .BracketClose) depth -= 1;
                    }
                    // Skip past the brackets
                    i = j + 1;
                    // std.debug.print("POST BRACKET: {} {} {any} {any}", .{ i, j, tokens[j..], tokens[i..] });
                },
                .OpPlus, .OpMinus, .OpMult, .OpDiv, .OpPow => {
                    // Create a node corresponding to the operator (its simply operates on its children, if they are operators themselves, its resolved recursively)
                    const new_root = try self.allocator.create(Node);
                    new_root.a = null;
                    new_root.b = null;
                    new_root.value = tokens[i];
                    var walker_null = self.root;
                    // Walk until we encounter a null node
                    while (walker_null) |walker| {
                        // If the current node is an operator, check if its precedence is higher or lower
                        if (walker.value.is_oper()) {
                            // If the precedence is higher, and the current node is not atomic
                            if (walker.value.precedence() < new_root.value.precedence() and !walker.atomic) {
                                // We check if the right child is empty or not, if not
                                if (walker.b != null and walker.b.?.value.is_oper()) {
                                    // then we check the precedence of the child if its an operator
                                    if (walker.b.?.value.precedence() < new_root.value.precedence()) {
                                        // if its less than the operator, we slide down the right node hoping to place it as the child of the current child
                                        walker_null = walker.b;
                                    } else {
                                        // Else we simply displace the child to the left ndoe of the new operator and place the new operator in its place
                                        new_root.a = walker.b;
                                        walker.b = new_root;
                                        break;
                                    }
                                } else {
                                    // if not an operator we simply slide the operator in its place and make the number its child
                                    new_root.a = walker.b;
                                    walker.b = new_root;
                                    break;
                                }
                            } else {
                                // if the precedence is lower or equal, we move the entire tree as the left node of the new operator and change the root to the new operator
                                new_root.a = walker;
                                self.root = new_root;
                                break;
                            }
                        } else {
                            // If not an operator simply place it as the root moving the number to its left node
                            new_root.a = walker;
                            self.root = new_root;
                            break;
                        }
                    }

                    // Legacy mostly working code (left here incase i fuckup the above working code my version controlling skills are kinda eh eh)
                    // if (self.root.?.value.is_oper()) {
                    // if (self.root.?.value.precedence() < new_root.value.precedence()) {
                    // var parent = side_null;
                    // while (side_null) |side| {
                    // if (side.value.is_oper()) {
                    // if (side.value.precedence() < new_root.value.precedence()) {
                    // parent = side_null;
                    // side_null = side.b;
                    //                                     } else {
                    //                                       new_root.a = side;
                    //                                     parent.?.b = new_root;
                    //                                   break;
                    //                             }
                    //                       } else {
                    //                         new_root.a = side.b;
                    //                       side.b = new_root;
                    // break;
                    //                 }
                    //           }
                    //     } else {
                    //       new_root.a = self.root;
                    //     self.root = new_root;
                    //                         }
                    //                   } else {
                    //                     new_root.a = self.root;
                    //                   self.root = new_root;
                    //             }
                    i += 1;
                },
                .Number => {
                    const new_node = try self.allocator.create(Node);
                    // Numbers are leave nodes, they may not have any nodes
                    new_node.a = null;
                    new_node.b = null;
                    new_node.value = tokens[i];
                    if (self.root) |root| {
                        if (root.b == null) {
                            root.b = new_node;
                        } else {
                            var walker_null = root.b;

                            while (walker_null) |walker| {
                                if (walker.a == null) {
                                    walker.a = new_node;
                                    break;
                                } else if (walker.b == null) {
                                    walker.b = new_node;
                                    break;
                                } else {
                                    walker_null = walker.b;
                                }
                            }
                        }
                    } else {
                        self.root = new_node;
                    }
                    i += 1;
                },
                else => {
                    i += 1;
                },
            }
        }
    }

    pub fn deinit(self: Self) void {
        if (self.root) |root| {
            root.cleanup(self.allocator);
        }
    }
};

const Parser = struct {
    tokens: []const Token,
    allocator: Allocator,

    const Self = @This();

    fn init(allocator: Allocator, tokens: []const Token) Self {
        return .{
            .tokens = tokens,
            .allocator = allocator,
        };
    }

    fn parse(self: *Self) !Tree {
        // Create the tree struct, and pass the tokens to the token tree
        var abstract_syntax_tree = Tree.init(self.allocator);
        try abstract_syntax_tree.parse(self.tokens);
        return abstract_syntax_tree;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();
    const stdin = std.io.getStdIn().reader();

    // const code = "def fun(a,b):\n  print(a,b)\nfun(54,15)";
    while (true) {
        var code: [1024]u8 = undefined;
        std.debug.print("> ", .{});
        _ = try stdin.readUntilDelimiter(&code, '\n');

        var lexer = Lexer.init(allocator, &code);
        const tokens = try lexer.lex();
        var parser = Parser.init(allocator, tokens.items);
        // Produce AST
        const tree = try parser.parse();

        defer tree.deinit();

        // tree.root.?.print();
        // Evaluate the tree
        const value = tree.root.?.eval();

        defer tokens.deinit();

        std.debug.print("\n{d}\n", .{value});

        // for (tokens.items) |tkn| {
        // switch (tkn) {
        // .Identifier => |str| std.debug.print("Identifier: {s}\n", .{str}),
        // else => std.debug.print("{any}\n", .{tkn}),
        // }
        // }
    }
}

const std = @import("std");

pub fn main() !void {
    std.debug.print("{d}\n", .{solve(@embedFile("input01.txt"))});
}

fn solve(input: []const u8) u32 {
    var sum: u32 = 0;
    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        if (std.mem.eql(u8, line, "")) {
            break;
        }

        var left: u32 = 0;

        outer: for (0..line.len) |start| {
            for (start..line.len) |end| {
                if (end-start > 5) { break; }
                var sub = line[start..end + 1];

                std.debug.print("<{s}>\n", .{sub});

                if (number(sub)) |n| {
                    left = n;
                    break :outer;
                }
            }
        }

        var right: u32 = 0;
        var start = line.len;
        outer: while (start >= 0) : (start -= 1) {
            for (start..line.len) |end| {
                if (end-start > 5) { break; }

                var sub = line[start..end + 1];

                std.debug.print("-{s}-\n", .{sub});
                
                if (number(sub)) |n| {
                    right = n;
                    break :outer;
                }
            }
        }

                
        std.debug.print("{s} {d} {d}\n", .{line, left, right});

        std.debug.print("{d}\n", .{left * 10 + right});
        sum += left * 10 + right;
    }

    return sum;
}

fn number(sub: []const u8) ?u32 {
    if (std.mem.eql(u8, "one", sub))    { return 1; }
    if (std.mem.eql(u8, "two", sub))    { return 2; }
    if (std.mem.eql(u8, "three", sub))  { return 3; }
    if (std.mem.eql(u8, "four", sub))   { return 4; }
    if (std.mem.eql(u8, "five", sub))   { return 5; }
    if (std.mem.eql(u8, "six", sub))    { return 6; }
    if (std.mem.eql(u8, "seven", sub))  { return 7; }
    if (std.mem.eql(u8, "eight", sub))  { return 8; }
    if (std.mem.eql(u8, "nine", sub))   { return 9; }
    if (std.mem.eql(u8, "1", sub))      { return 1; }
    if (std.mem.eql(u8, "2", sub))      { return 2; }
    if (std.mem.eql(u8, "3", sub))      { return 3; }
    if (std.mem.eql(u8, "4", sub))      { return 4; }
    if (std.mem.eql(u8, "5", sub))      { return 5; }
    if (std.mem.eql(u8, "6", sub))      { return 6; }
    if (std.mem.eql(u8, "7", sub))      { return 7; }
    if (std.mem.eql(u8, "8", sub))      { return 8; }
    if (std.mem.eql(u8, "9", sub))      { return 9; }
    return null;
}

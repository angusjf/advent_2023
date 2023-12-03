const std = @import("std");

pub fn main() !void {
    std.debug.print("total: {d}\n", .{solve(@embedFile("input02.txt"))});
}

fn solve(input: []const u8) u32 {
    var lines = std.mem.split(u8, input, "\n");
    var total: u32 = 0;

    while (lines.next()) |line| {
        var red: u32 = 0;
        var green: u32 = 0;
        var blue: u32 = 0;
        if (line.len == 0) {
            break;
        }
        var y = std.mem.split(u8, line, ":");
        _ = y.next() orelse return 0;
        var round = y.next() orelse return 0;
        var games = std.mem.split(u8, round, ";");
        while (games.next()) |game| {
            var moves = std.mem.split(u8, game, ",");
            while (moves.next()) |move| {
                var parts = std.mem.split(u8, move[1..], " ");
                var ns = parts.next() orelse return 0;
                var n = std.fmt.parseInt(u32, ns, 10) catch return 0;
                var c = parts.next() orelse return 0;
                if (std.mem.eql(u8, c, "red")) {
                    red = @max(red, n);
                }
                if (std.mem.eql(u8, c, "green")) {
                    green = @max(green, n);
                }
                if (std.mem.eql(u8, c, "blue")) {
                    blue = @max(blue, n);
                }
            }
        }
        std.debug.print("r:{d} g:{d} b:{d}\n", .{red, green, blue});
        total += red * blue * green;
        std.debug.print("total: {d}\n", .{total});
    }

    return total;
}

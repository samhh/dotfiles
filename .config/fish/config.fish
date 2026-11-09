if status is-interactive
    set -g fish_greeting
end

if not set -q VISUAL; and command -q hx
    set -gx VISUAL hx
end

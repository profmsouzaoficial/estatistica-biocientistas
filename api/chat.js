export default async function handler(req, res) {
  // 1. Bloqueia se não for POST
  if (req.method !== 'POST') {
    return res.status(405).json({ error: 'Método não permitido.' });
  }

  // 2. Verifica a chave da DeepSeek
  const apiKey = process.env.DEEPSEEK_API_KEY;
  if (!apiKey) {
    return res.status(200).json({ reply: "Erro 🔴: A variável DEEPSEEK_API_KEY não foi encontrada na Vercel." });
  }

  const { message } = req.body;
  if (!message) {
    return res.status(200).json({ reply: "Erro: Nenhuma mensagem recebida." });
  }

  const systemPrompt = `Você é um tutor socrático especialista em Bioestatística. 
  Seu objetivo é ajudar alunos da área da saúde a entenderem estatística aplicada com R. 
  Nunca dê a resposta direta de imediato. Faça perguntas que guiem o aluno ao raciocínio correto.`;

  try {
    // 3. Chamada para a API da DeepSeek
    const response = await fetch('https://api.deepseek.com/chat/completions', {
      method: 'POST',
      headers: { 
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${apiKey}` // A DeepSeek usa Bearer token
      },
      body: JSON.stringify({
        model: 'deepseek-chat', // ou 'deepseek-coder' se preferir focar só no código R
        messages: [
          { role: 'system', content: systemPrompt },
          { role: 'user', content: message }
        ],
        temperature: 0.7
      })
    });

    const data = await response.json();
    
    // 4. Captura erros (ex: saldo esgotado, chave errada)
    if (!response.ok) {
      console.error("Erro da DeepSeek:", data);
      return res.status(200).json({ reply: `**Erro da API DeepSeek:** ${data.error?.message || 'Motivo desconhecido.'}` });
    }

    // 5. Extrai a resposta no formato da DeepSeek
    if (data.choices && data.choices.length > 0) {
      const reply = data.choices[0].message.content;
      return res.status(200).json({ reply: reply });
    } else {
      return res.status(200).json({ reply: "A DeepSeek devolveu uma resposta vazia." });
    }

  } catch (error) {
    console.error("Erro no Vercel:", error);
    return res.status(200).json({ reply: `**Erro no Servidor:** ${error.message}` });
  }
}
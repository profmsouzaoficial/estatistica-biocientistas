export default async function handler(req, res) {
  // Bloqueia requisições que não sejam POST
  if (req.method !== 'POST') {
    return res.status(405).json({ error: 'Método não permitido.' });
  }

  // A chave de API fica protegida e invisível para o utilizador
  const apiKey = process.env.GEMINI_API_KEY;
  const { message } = req.body;

  if (!message) {
    return res.status(400).json({ error: 'Mensagem vazia.' });
  }

  // O System Prompt que define a "personalidade" do seu tutor
  const systemInstruction = `Você é o tutor virtual oficial do livro de Bioestatística (https://profmsouzaoficial.github.io/estatistica-biocientistas) do Professor Márcio Souza, da Universidade Federal de Juiz de Fora (UFJF-GV). Seu papel é ajudar estudantes de Odontologia, Farmácia e Fisioterapia com dúvidas sobre R, Estatística e Análise de Dados. Seja sempre encorajador, didático e use exemplos da área da saúde quando aplicável. Se o aluno pedir a resposta para um código ou cálculo, não dê a resposta pronta: aja de forma socrática, dando dicas ou trechos incompletos para que ele mesmo chegue à conclusão. Responda de forma concisa e formate suas mensagens em Markdown (usando negritos e blocos de código quando necessário). Caso o aluno pergunte coisas que fogem do escopo do livro, favor informar, educamente, que essa demanda não é possível de ser atendida por esse canal (peça que o aluno me procure pessoalmente para tratar do referente assunto).`;

  try {
    // Comunicação direta com a API oficial do Gemini (Sem precisar de pacotes npm)
    const response = await fetch(`https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=${apiKey}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        system_instruction: { parts: [{ text: systemInstruction }] },
        contents: [{ role: "user", parts: [{ text: message }] }]
      })
    });

    const data = await response.json();
    
    if (!response.ok) {
      throw new Error(data.error?.message || 'Erro na API do Gemini');
    }

    const reply = data.candidates[0].content.parts[0].text;

    // Resposta enviada de volta para o navegador do aluno
    res.status(200).json({ reply });

  } catch (error) {
    console.error("Erro no Chatbot:", error);
    res.status(500).json({ error: 'Ocorreu um erro ao processar a sua dúvida. Tente novamente.' });
  }
}